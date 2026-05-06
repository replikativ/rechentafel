(ns rechentafel.fn.lookup
  "Lookup & reference functions (POI category: lookup — 11 fns).

  These are the trickiest in the library because they take *ranges*
  (area values) as inputs and return scalars or subranges. We work
  against our tagged `:area` shape: `{:t :area :values [[row0] [row1]]
  :r0 :c0 :r1 :c1 :sheet}`.

  Volatile fns (INDIRECT / OFFSET) want the evaluator context to
  resolve references after the fact; we register them lazily. The
  strict lookups (VLOOKUP/HLOOKUP/MATCH/INDEX/LOOKUP) just walk the
  area."
  (:require [clojure.string :as str]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Area helpers

(defn- area? [v] (= :area (:t v)))

(defn- area-rows [v]
  (cond
    (area? v) (:values v)
    (val/ref? v) [[(-> v :resolved (or val/BLANK))]]
    :else [[v]]))

(defn- area-shape [v]
  (cond
    (area? v) [(count (:values v))
               (count (first (:values v)))]
    :else [1 1]))

;; ---------------------------------------------------------------------------
;; Equality + ordering used by VLOOKUP/HLOOKUP/MATCH

(defn- values-equal?
  "POI-compatible equality for lookup comparisons. Case-insensitive for
  strings, numeric-equal for numbers, same-error for errors."
  [a b]
  (cond
    (and (val/err? a) (val/err? b)) (= (:v a) (:v b))
    (or (val/err? a) (val/err? b)) false
    (and (val/num? a) (val/num? b)) (== (double (:v a)) (double (:v b)))
    (and (val/bool? a) (val/bool? b)) (= (:v a) (:v b))
    (and (val/str? a) (val/str? b))
    (= (str/lower-case (:v a)) (str/lower-case (:v b)))
    ;; cross-type: coerce right toward left's type (POI's loose match)
    (val/num? a) (let [n (val/to-num b)] (and (val/num? n) (== (:v a) (:v n))))
    (val/str? a) (let [s (val/to-str b)] (and (val/str? s)
                                              (= (str/lower-case (:v a))
                                                 (str/lower-case (:v s)))))
    :else false))

(defn- compare-values
  "Ordering used by sorted-lookup — returns -1/0/1. Mixed-type follows
  POI's type-order: number < string < boolean."
  [a b]
  (cond
    (= (:t a) (:t b))
    (case (:t a)
      :num  (compare (double (:v a)) (double (:v b)))
      :str  (compare (str/lower-case (:v a)) (str/lower-case (:v b)))
      :bool (compare (boolean (:v a)) (boolean (:v b)))
      0)
    (val/num? a) -1
    (val/num? b) 1
    (val/str? a) -1
    (val/str? b) 1
    :else 0))

(defn- wildcard-match?
  "Case-insensitive wildcard match used by MATCH's exact mode when
  `match_type` = 0. Supports ? and *."
  [pattern s]
  (let [tgt (str/lower-case s)
        pat (-> (str/lower-case pattern)
                (str/replace #"[.^$+(){}\[\]\\|]" #(str "\\" %))
                (str/replace #"\?" ".")
                (str/replace #"\*" ".*"))
        re  (re-pattern (str "^" pat "$"))]
    (boolean (re-matches re tgt))))

;; ---------------------------------------------------------------------------
;; MATCH

(f/register! "MATCH"
  ;; MATCH(lookup_value, lookup_array, [match_type])
  ;;   1 (default): largest value <= lookup — lookup_array must be ascending
  ;;   0: exact match, allows wildcards when lookup_value is text
  ;;   -1: smallest value >= lookup — lookup_array must be descending
             (fn [args]
               (let [target (nth args 0)
                     area   (nth args 1)
                     mtype  (if (> (count args) 2) (long (f/num! (nth args 2))) 1)
                     rows   (area-rows area)
          ;; flatten into a 1-D sequence of [idx cell] pairs
                     flat   (let [cols (count (first rows))]
                              (if (> cols 1)
                     ;; row vector
                                (map-indexed vector (first rows))
                                (map-indexed vector (map first rows))))]
                 (case mtype
                   0 (let [wild? (val/str? target)]
                       (or (some (fn [[i v]]
                                   (when (or (values-equal? target v)
                                             (and wild? (val/str? v)
                                                  (wildcard-match? (:v target) (:v v))))
                                     (val/number (double (inc i)))))
                                 flat)
                           val/ERR-NA))
                   1 (let [r (volatile! nil)]
                       (doseq [[i v] flat]
                         (when (<= (compare-values v target) 0)
                           (vreset! r (inc i))))
                       (if-let [idx @r] (val/number (double idx)) val/ERR-NA))
                   -1 (let [r (volatile! nil)]
                        (doseq [[i v] flat]
                          (when (>= (compare-values v target) 0)
                            (vreset! r (inc i))))
                        (if-let [idx @r] (val/number (double idx)) val/ERR-NA))
                   (f/domain-error! :value))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; VLOOKUP / HLOOKUP

(defn- do-vlookup [target rows col-idx exact?]
  (let [col-idx (dec col-idx)
        cols (count (first rows))]
    (when (or (neg? col-idx) (>= col-idx cols)) (f/domain-error! :ref))
    (if exact?
      (or (some (fn [row]
                  (when (values-equal? target (first row))
                    (nth row col-idx)))
                rows)
          val/ERR-NA)
      ;; approximate match: largest first-column value <= target
      (let [best (volatile! nil)]
        (doseq [row rows]
          (when (<= (compare-values (first row) target) 0)
            (vreset! best row)))
        (if-let [row @best] (nth row col-idx) val/ERR-NA)))))

(f/register! "VLOOKUP"
             (fn [args]
               (let [target (nth args 0)
                     table  (nth args 1)
                     col    (long (f/num! (nth args 2)))
                     exact? (if (> (count args) 3) (not (f/bool! (nth args 3))) false)
                     rows   (area-rows table)]
                 (do-vlookup target rows col exact?)))
             :arity [3 4])

(f/register! "HLOOKUP"
             (fn [args]
               (let [target (nth args 0)
                     table  (nth args 1)
                     row-n  (long (f/num! (nth args 2)))
                     exact? (if (> (count args) 3) (not (f/bool! (nth args 3))) false)
                     rows   (area-rows table)
          ;; transpose: treat columns as records
                     cols   (apply mapv vector rows)]
                 (do-vlookup target cols row-n exact?)))
             :arity [3 4])

;; ---------------------------------------------------------------------------
;; LOOKUP (array form + vector form)

(f/register! "LOOKUP"
             (fn [args]
               (let [target (nth args 0)
                     a      (area-rows (nth args 1))
                     result (when (> (count args) 2) (area-rows (nth args 2)))]
                 (if result
        ;; Vector form: search row-or-column of a, return matching pos in result.
                   (let [search (if (= 1 (count a)) (first a) (mapv first a))
                         best (volatile! nil)
                         result-vec (if (= 1 (count result)) (first result) (mapv first result))]
                     (doseq [[i v] (map-indexed vector search)]
                       (when (<= (compare-values v target) 0)
                         (vreset! best i)))
                     (if-let [i @best]
                       (nth result-vec i val/ERR-NA)
                       val/ERR-NA))
        ;; Array form: 2-D → pick last column/row as the result.
                   (let [rows (count a)
                         cols (count (first a))
              ;; use the longer dimension as the search direction
                         horizontal? (> cols rows)
                         search (if horizontal?
                                  (first a)
                                  (mapv first a))
                         last-series (if horizontal?
                                       (last a)
                                       (mapv last a))
                         best (volatile! nil)]
                     (doseq [[i v] (map-indexed vector search)]
                       (when (<= (compare-values v target) 0)
                         (vreset! best i)))
                     (if-let [i @best]
                       (nth last-series i val/ERR-NA)
                       val/ERR-NA)))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; INDEX

(f/register! "INDEX"
  ;; INDEX(array, row_num, [column_num])
  ;; row_num = 0 returns the whole column; column_num = 0 returns the whole row.
             (fn [args]
               (let [arr (nth args 0)
                     rows (area-rows arr)
                     rn (long (f/num! (nth args 1)))
                     cn (if (> (count args) 2) (long (f/num! (nth args 2))) 1)
                     [nrows ncols] [(count rows) (count (first rows))]]
                 (when (or (neg? rn) (> rn nrows)) (f/domain-error! :ref))
                 (when (or (neg? cn) (> cn ncols)) (f/domain-error! :ref))
                 (cond
                   (and (zero? rn) (zero? cn)) arr
                   (zero? rn) ;; entire column cn
                   {:t :area :r0 0 :c0 (dec cn) :r1 (dec nrows) :c1 (dec cn)
                    :values (mapv (fn [row] [(nth row (dec cn))]) rows)}
                   (zero? cn) ;; entire row rn
                   {:t :area :r0 (dec rn) :c0 0 :r1 (dec rn) :c1 (dec ncols)
                    :values [(nth rows (dec rn))]}
                   :else
                   (nth (nth rows (dec rn)) (dec cn)))))
             :arity [2 4])

;; ---------------------------------------------------------------------------
;; XLOOKUP / XMATCH

(f/register! "XLOOKUP"
  ;; XLOOKUP(lookup, lookup_array, return_array,
  ;;         [if_not_found], [match_mode=0], [search_mode=1])
  ;; match_mode: 0 exact, -1 exact-or-next-smaller, 1 exact-or-next-larger,
  ;;             2 wildcard.
  ;; search_mode: 1 first-to-last, -1 last-to-first, 2 binary asc, -2 bin desc.
             (fn [args]
               (let [target (nth args 0)
                     la     (area-rows (nth args 1))
                     ra     (area-rows (nth args 2))
                     if-nf  (when (> (count args) 3) (nth args 3))
                     mmode  (if (> (count args) 4) (long (f/num! (nth args 4))) 0)
                     smode  (if (> (count args) 5) (long (f/num! (nth args 5))) 1)
                     search (if (= 1 (count la)) (first la) (mapv first la))
                     return (if (= 1 (count ra)) (first ra) (mapv first ra))
                     indexed (map-indexed vector search)
                     indexed (if (neg? smode) (reverse indexed) indexed)
                     match-fn (case mmode
                                0 (fn [v] (values-equal? target v))
                                2 (fn [v] (and (val/str? target) (val/str? v)
                                               (wildcard-match? (:v target) (:v v))))
                                -1 (fn [v] (values-equal? target v))
                                1  (fn [v] (values-equal? target v))
                                (f/domain-error! :value))
                     hit (some (fn [[i v]] (when (match-fn v) i)) indexed)]
                 (cond
                   hit (nth return hit val/ERR-NA)
                   (= mmode 0) (or if-nf val/ERR-NA)
                   (or (= mmode -1) (= mmode 1))
                   (let [best (volatile! nil)]
                     (doseq [[i v] (map-indexed vector search)]
                       (let [c (compare-values v target)]
                         (cond
                           (and (= mmode -1) (<= c 0)) (vreset! best i)
                           (and (= mmode 1)  (>= c 0) (nil? @best)) (vreset! best i))))
                     (if-let [i @best] (nth return i val/ERR-NA)
                             (or if-nf val/ERR-NA)))
                   :else (or if-nf val/ERR-NA))))
             :arity [3 6])

(f/register! "XMATCH"
  ;; Same modes as XLOOKUP but returns position.
             (fn [args]
               (let [target (nth args 0)
                     la     (area-rows (nth args 1))
                     mmode  (if (> (count args) 2) (long (f/num! (nth args 2))) 0)
                     smode  (if (> (count args) 3) (long (f/num! (nth args 3))) 1)
                     search (if (= 1 (count la)) (first la) (mapv first la))
                     indexed (map-indexed vector search)
                     indexed (if (neg? smode) (reverse indexed) indexed)
                     match-fn (case mmode
                                0 (fn [v] (values-equal? target v))
                                2 (fn [v] (and (val/str? target) (val/str? v)
                                               (wildcard-match? (:v target) (:v v))))
                                -1 (fn [v] (values-equal? target v))
                                1  (fn [v] (values-equal? target v))
                                (f/domain-error! :value))
                     hit (some (fn [[i v]] (when (match-fn v) i)) indexed)]
                 (cond
                   hit (val/number (double (inc hit)))
                   :else val/ERR-NA)))
             :arity [2 4])

;; ---------------------------------------------------------------------------
;; HYPERLINK — returns the display text (second arg), or the URL if only one.

(f/register! "HYPERLINK"
             (fn [args]
               (if (>= (count args) 2)
                 (nth args 1)
                 (nth args 0)))
             :arity [1 2])

;; ---------------------------------------------------------------------------
;; Volatile — INDIRECT / OFFSET / GETPIVOTDATA
;;
;; INDIRECT and OFFSET need the evaluator context — they construct
;; references at runtime. They receive ctx with :eval (evaluates an AST
;; to a value), :parse (string → AST), :resolve-area (takes a :ref/:range
;; AST and returns an :area value). Registered lazy so they can either
;; see the AST (OFFSET wants the un-evaluated reference) or evaluate
;; args themselves (INDIRECT needs the string).

(defn- eval1 [ctx ast]
  (if-let [ev (:eval ctx)]
    (ev ctx ast) ast))

(defn- ast->ref-coords
  "Extract {:sheet :r0 :c0 :r1 :c1} from a :ref/:range AST node.
  Returns nil if ast is not a reference shape."
  [ast]
  (case (:op ast)
    :ref   {:sheet (:sheet ast)
            :r0 (:row ast) :c0 (:col ast)
            :r1 (:row ast) :c1 (:col ast)}
    :range (let [l (:left ast) r (:right ast)]
             {:sheet (:sheet l)
              :r0 (min (:row l 0) (:row r 0))
              :c0 (min (:col l 0) (:col r 0))
              :r1 (max (:row l 0) (:row r 0))
              :c1 (max (:col l 0) (:col r 0))})
    nil))

(f/register! "OFFSET"
  ;; OFFSET(ref, rows, cols, [height], [width]) — shift the reference
  ;; by (rows, cols); optionally resize to (height, width). Height/width
  ;; default to the reference's own dimensions. Returns an area that
  ;; the evaluator then resolves into values.
             (fn [ctx ast-args]
               (let [ref-ast (first ast-args)
                     rows    (long (f/num! (eval1 ctx (nth ast-args 1))))
                     cols    (long (f/num! (eval1 ctx (nth ast-args 2))))
                     coords  (ast->ref-coords ref-ast)]
                 (if-not coords
                   val/ERR-VALUE
                   (let [h-def (inc (- (long (:r1 coords)) (long (:r0 coords))))
                         w-def (inc (- (long (:c1 coords)) (long (:c0 coords))))
                         h     (if (>= (count ast-args) 4)
                                 (long (f/num! (eval1 ctx (nth ast-args 3))))
                                 h-def)
                         w     (if (>= (count ast-args) 5)
                                 (long (f/num! (eval1 ctx (nth ast-args 4))))
                                 w-def)
                         r0    (+ (long (:r0 coords)) rows)
                         c0    (+ (long (:c0 coords)) cols)
                         r1    (+ r0 (dec h))
                         c1    (+ c0 (dec w))]
                     (if (or (neg? r0) (neg? c0) (< r1 r0) (< c1 c0))
                       val/ERR-REF
                       (if-let [resolve-area (:resolve-area ctx)]
                         (resolve-area ctx {:sheet (:sheet coords)
                                            :r0 r0 :c0 c0 :r1 r1 :c1 c1})
                         val/ERR-REF))))))
             :arity [3 5] :lazy? true :volatile? true)

(f/register! "INDIRECT"
  ;; INDIRECT(ref_text, [a1_style]) — parse the string as a ref/range
  ;; and resolve it. Requires :parse and :resolve-area in ctx.
             (fn [ctx ast-args]
               (let [s  (val/to-str (eval1 ctx (first ast-args)))
                     parse (:parse ctx)
                     resolve-area (:resolve-area ctx)]
                 (cond
                   (not (val/str? s))       val/ERR-REF
                   (nil? parse)             val/ERR-REF
                   (nil? resolve-area)      val/ERR-REF
                   :else
                   (try
                     (let [ast (parse (:v s))
                           coords (ast->ref-coords ast)]
                       (if coords
                         (resolve-area ctx coords)
                         val/ERR-REF))
                     (catch #?(:clj Throwable :cljs :default) _ val/ERR-REF)))))
             :arity [1 2] :lazy? true :volatile? true)

(f/register! "GETPIVOTDATA"
             (fn [_args] val/ERR-NA)
             :arity [2 nil])
