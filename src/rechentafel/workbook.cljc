(ns rechentafel.workbook
  "Workbook state protocols and prototype backings.

  We keep value storage pluggable so the evaluator (dep graph, dirty
  propagation, eval loop) can be written against an interface and later
  swapped from a plain persistent map to a stratum-column-backed store.

  Three backings are prototyped:

    :phm   — persistent hash-map  {cell-id → tagged-value}
    :thm   — transient hash-map   (same, but mutable for bulk load)
    :cols  — primitive columns    type:byte[] + num:double[] + str-id:int[]
                                  per sheet, indexed by dense (row*W + col)

  The `:cols` backing is the cheap sketch of an eventual stratum column
  layout: type tag + parallel primitive arrays, no per-cell map wrapper,
  SIMD-friendly shape."
  (:require [rechentafel.value :as val]
            [rechentafel.cell :as cell]))

;; ---------------------------------------------------------------------------
;; Protocol

(defprotocol IValues
  (-get   [this id]     "Return tagged value (BLANK if unset).")
  (-put!  [this id v]   "Mutating set (transient-like).")
  (-put   [this id v]   "Persistent set.")
  (-count [this]        "Number of non-blank cells.")
  (-forked [this]       "O(1) fork. New store shares structure."))

(defprotocol ITransient
  (-finish [this] "Convert a transient store to a persistent one."))

;; ---------------------------------------------------------------------------
;; :phm — persistent hash-map

(defrecord PHMStore [m]
  IValues
  (-get   [_ id]   (get m id val/BLANK))
  (-put!  [_ _ _]  (throw (ex-info "persistent PHM not mutable" {})))
  (-put   [_ id v] (->PHMStore (assoc m id v)))
  (-count [_]      (count m))
  (-forked [this]  this))

(defn phm-store [] (->PHMStore {}))

;; ---------------------------------------------------------------------------
;; :thm — transient map wrapper

(deftype THMStore [^:unsynchronized-mutable m]
  IValues
  (-get   [_ id]   (get m id val/BLANK))
  (-put!  [this id v] (set! m (assoc! m id v)) this)
  (-put   [_ _ _]  (throw (ex-info "transient THM doesn't support persistent put" {})))
  (-count [_]      (count m))
  (-forked [_]     (throw (ex-info "cannot fork transient store; persist first" {})))
  ITransient
  (-finish [_] (->PHMStore (persistent! m))))

(defn thm-store [] (->THMStore (transient {})))

;; ---------------------------------------------------------------------------
;; :cols — primitive columns, single-sheet dense grid

;; Type-byte codes: match the set of tagged-value :t keys we care about.
(def ^:const T-BLANK 0)
(def ^:const T-NUM   1)
(def ^:const T-STR   2)
(def ^:const T-BOOL  3)
(def ^:const T-ERR   4)

(def err-codes  {:null 0 :div0 1 :value 2 :ref 3 :name 4 :num 5 :na 6 :getting-data 7})
(def ^:private err-lookup (into {} (map (fn [[k v]] [v k])) err-codes))

(defn- tagged->cols [v]
  (case (:t v)
    :num   [T-NUM (double (:v v)) 0]
    :bool  [T-BOOL (if (:v v) 1.0 0.0) 0]
    :err   [T-ERR 0.0 (long (err-codes (:v v) 0))]
    :str   [T-STR 0.0 -1]                ;; str-id assigned at call site
    :blank [T-BLANK 0.0 0]
    [T-BLANK 0.0 0]))

(defn- cols->tagged [^long t ^double num ^long sid str-pool]
  (case t
    0 val/BLANK
    1 (val/number num)
    2 (val/string (if (neg? sid) "" (nth str-pool sid)))
    3 (if (zero? num) val/FALSE val/TRUE)
    4 {:t :err :v (get err-lookup sid :value)}
    val/BLANK))

;; ColsStore is a JVM-only prototype using primitive byte/double/int
;; arrays and a java.util.ArrayList string pool. Not used by the
;; evaluator (which goes through MTV); kept as a benchmarking target.
;; Gated behind :clj so cljs compilation doesn't trip on the
;; primitive-array interop.
#?(:clj
   (do
     (deftype ColsStore [^long width
                         ^long height
                         ^bytes types
                         ^doubles nums
                         ^ints str-ids
                         ^:unsynchronized-mutable str-pool  ;; java.util.ArrayList
                         ^:unsynchronized-mutable count-nz]
       IValues
       (-get [_ id]
         (let [r (cell/row id) c (cell/col id)
               off (+ (* r width) c)]
           (cols->tagged (aget types off) (aget nums off) (aget str-ids off) str-pool)))
       (-put! [this id v]
         (let [r (cell/row id) c (cell/col id)
               off (+ (* r width) c)
               prev-t (aget types off)
               [t num sid] (tagged->cols v)
               sid (if (= t T-STR)
                     (let [^java.util.ArrayList p str-pool
                           n (.size p)]
                       (.add p ^String (:v v))
                       n)
                     sid)]
           (aset types off (byte t))
           (aset nums off (double num))
           (aset str-ids off (int sid))
           (when (and (= prev-t T-BLANK) (not= t T-BLANK))
             (set! count-nz (inc count-nz)))
           (when (and (not= prev-t T-BLANK) (= t T-BLANK))
             (set! count-nz (dec count-nz)))
           this))
       (-put [_ _ _] (throw (ex-info "ColsStore is mutable-only in this prototype" {})))
       (-count [_] count-nz)
       (-forked [this]
         (let [t2 (byte-array (alength types))
               n2 (double-array (alength nums))
               s2 (int-array  (alength str-ids))]
           (System/arraycopy types   0 t2 0 (alength types))
           (System/arraycopy nums    0 n2 0 (alength nums))
           (System/arraycopy str-ids 0 s2 0 (alength str-ids))
           (ColsStore. width height t2 n2 s2 (java.util.ArrayList. ^java.util.ArrayList str-pool) count-nz))))

     (defn cols-store [^long width ^long height]
       (let [n (* width height)]
         (ColsStore. width height
                     (byte-array   n)
                     (double-array n)
                     (int-array    n)
                     (java.util.ArrayList.)
                     0)))))
