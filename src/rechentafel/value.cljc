(ns rechentafel.value
  "Tagged value types for the cljc formula engine. Shaped to match POI's
  ValueEval hierarchy semantically without coupling to Java types.

  Every value in the engine is a plain Clojure map with a `:t` tag:
    {:t :num   :v 42.0}
    {:t :str   :v \"abc\"}
    {:t :bool  :v true}
    {:t :blank}
    {:t :err   :v :value}            ;; :null :div0 :value :ref :name :num :na :getting-data
    {:t :ref   :sheet \"S\" :row 0 :col 0}
    {:t :area  :sheet \"S\" :r0 0 :c0 0 :r1 4 :c1 2 :values [[...] [...]]}

  Values are Datahike-safe by construction — all plain data."
  (:require [clojure.string :as str]))

(def error-codes
  "Numeric codes POI uses; kept for round-trip with the legacy engine."
  {:null  0
   :div0  7
   :value 15
   :ref   23
   :name  29
   :num   36
   :na    42
   :getting-data 43})

(def code->error (into {} (map (fn [[k v]] [v k])) error-codes))

(defn num?  [v] (= :num  (:t v)))
(defn str?  [v] (= :str  (:t v)))
(defn bool? [v] (= :bool (:t v)))
(defn blank? [v] (= :blank (:t v)))
(defn err?  [v] (= :err  (:t v)))
(defn ref?  [v] (= :ref  (:t v)))
(defn area? [v] (= :area (:t v)))

(def BLANK {:t :blank})
(def TRUE  {:t :bool :v true})
(def FALSE {:t :bool :v false})

(defn number [n] {:t :num :v (double n)})
(defn string [s] {:t :str :v s})
(defn boolean-v [b] {:t :bool :v (boolean b)})
(defn error [code] {:t :err :v code})

(def ERR-NULL  (error :null))
(def ERR-DIV0  (error :div0))
(def ERR-VALUE (error :value))
(def ERR-REF   (error :ref))
(def ERR-NAME  (error :name))
(def ERR-NUM   (error :num))
(def ERR-NA    (error :na))
;; Modern dynamic-array errors. #SPILL! marks an anchor whose array
;; result couldn't fill its target rectangle (blocked, beyond edge,
;; merged, table cell, OOM, indeterminate). #CALC! covers nested-array
;; / empty-array pathologies that arise specifically from dynamic
;; array evaluation.
(def ERR-SPILL (error :spill))
(def ERR-CALC  (error :calc))

;; LAMBDA support: a `:lambda` value carries its parameter list, body
;; AST, and captured lexical environment. `:omitted` is the sentinel
;; bound to optional params not supplied at the call site (detected by
;; `ISOMITTED`).
(def OMITTED {:t :omitted})

(defn lambda? [v] (= :lambda (:t v)))
(defn omitted? [v] (= :omitted (:t v)))

;; ---------------------------------------------------------------------------
;; Coercion — mirrors POI's OperandResolver semantics.
;;
;; Excel rules:
;;   blank  → 0 (numeric), "" (string), FALSE (bool)
;;   bool   → 1/0 (numeric), "TRUE"/"FALSE" (string)
;;   string → parse as number when coerced to number; error if not parsable
;;   error  → propagates

(defn- parse-number [s]
  #?(:clj  (try (Double/parseDouble s) (catch Exception _ nil))
     :cljs (let [n (js/parseFloat s)] (when-not (js/isNaN n) n))))

(defn first-cell-of
  "Returns the scalar at row 0 col 0 of an Area value, or BLANK if empty."
  [area]
  (or (get-in (:values area) [0 0]) BLANK))

(defn to-num
  "Coerce to {:t :num} or propagate :err."
  [v]
  (case (:t v)
    :num   v
    :bool  (number (if (:v v) 1.0 0.0))
    :blank (number 0.0)
    :str   (if-let [n (parse-number (:v v))] (number n) ERR-VALUE)
    :err   v
    :area  (to-num (first-cell-of v))
    :ref   (to-num (:resolved v ERR-VALUE))
    ERR-VALUE))

(defn to-double
  "Unwrap to a raw double. Throws (JVM) / NaN (cljs) when :err."
  ^double [v]
  (let [n (to-num v)]
    (if (num? n)
      (double (:v n))
      #?(:clj  (throw (ex-info "coerce-to-double failed" {:value v}))
         :cljs js/NaN))))

(defn to-str [v]
  (case (:t v)
    :str   v
    :num   (let [d (:v v)
                 i (long d)]
             (string (if (== (double i) d) (str i) (str d))))
    :bool  (string (if (:v v) "TRUE" "FALSE"))
    :blank (string "")
    :err   v
    (string (pr-str v))))

(defn to-bool [v]
  (case (:t v)
    :bool  v
    :num   (boolean-v (not (zero? (:v v))))
    :str   (case (some-> (:v v) str/lower-case)
             "true"  TRUE
             "false" FALSE
             ERR-VALUE)
    :blank FALSE
    :err   v
    ERR-VALUE))

(defn truthy?
  "Excel truthiness for IF/AND/OR — coerces through to-bool."
  [v]
  (let [b (to-bool v)]
    (and (bool? b) (:v b))))
