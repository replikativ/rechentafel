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
  "Equality for lookup comparisons, as Excel's: case-insensitive for
  strings, numeric-equal for numbers, same-error for errors, never across
  types."
  [a b]
  (cond
    (and (val/err? a) (val/err? b)) (= (:v a) (:v b))
    (or (val/err? a) (val/err? b)) false
    (and (val/num? a) (val/num? b)) (== (double (:v a)) (double (:v b)))
    (and (val/bool? a) (val/bool? b)) (= (:v a) (:v b))
    (and (val/str? a) (val/str? b))
    (= (str/lower-case (:v a)) (str/lower-case (:v b)))
    ;; as Excel's lookups: text never equals a number ("1" is not 1), and
    ;; "" is not an empty cell (MATCH("",A:A,0) finds none)
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

;; Excel's approximate search looks only at values of the target's type:
;; errors, blanks and other types are skipped, so MATCH(2,1/(cond)) finds
;; the last match
(defn- same-kind? [target v]
  (cond (val/num? target) (val/num? v)
        (val/str? target) (val/str? v)
        (val/bool? target) (val/bool? v)
        :else false))

(defn- approx-position
  "The 1-based position a binary search over `flat` ([idx cell] pairs)
   lands on for `target`, stepping right while `(ok? (compare v target) 0)`;
   #N/A when there is none."
  [flat target ok?]
  (let [vs (filterv #(same-kind? target (second %)) flat)
        l (loop [l 0 r (count vs)]
            (if (< l r)
              (let [m (quot (+ l r) 2)]
                (if (ok? (compare-values (second (nth vs m)) target) 0) (recur (inc m) r) (recur l m)))
              l))]
    (if (zero? l) val/ERR-NA (val/number (double (inc (first (nth vs (dec l)))))))))

;; ---------------------------------------------------------------------------
;; MATCH

(f/register! "MATCH"
  ;; MATCH(lookup_value, lookup_array, [match_type])
  ;;   1 (default): largest value <= lookup — lookup_array must be ascending
  ;;   0: exact match, allows wildcards when lookup_value is text
  ;;   -1: smallest value >= lookup — lookup_array must be descending
             (fn match-fn [args]
               (if (and (f/area? (nth args 0))
                        (> (count (apply concat (:values (nth args 0)))) 1))
                 ;; an array of lookup values: one MATCH each, as Excel lifts it
                 (let [a (nth args 0)]
                   (-> (dissoc a :sheet :ref-r0 :ref-r1 :ref-c0 :ref-c1)
                       (update :values (fn [rows] (mapv (fn [row] (mapv #(match-fn (assoc (vec args) 0 %)) row)) rows)))))
                 (let [target (let [t (nth args 0)] (if (f/area? t) (get-in (:values t) [0 0] val/BLANK) t))
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
                   ;; approximate: a binary search, as Excel's (and IronCalc's
                   ;; binary_search): on sorted data the position of the
                   ;; largest value <= target; on unsorted data wherever the
                   ;; search lands, not a linear scan's answer
                     1 (approx-position flat target <=)
                     -1 (approx-position flat target >=)
                     (f/domain-error! :value)))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; VLOOKUP / HLOOKUP

(defn- do-vlookup [target rows col-idx exact?]
  (let [col-idx (dec col-idx)
        cols (count (first rows))]
    (when (or (neg? col-idx) (>= col-idx cols)) (f/domain-error! :ref))
    (if exact?
      ;; text may hold wildcards (* ? ~), as in MATCH type 0
      (let [wild? (val/str? target)]
        (or (some (fn [row]
                    (when (or (values-equal? target (first row))
                              (and wild? (val/str? (first row)) (wildcard-match? (:v target) (:v (first row)))))
                      (nth row col-idx)))
                  rows)
            val/ERR-NA))
      ;; approximate match: the binary search over the first column
      (let [pos (approx-position (map-indexed (fn [i row] [i (first row)]) rows) target <=)]
        (if (val/err? pos) pos (nth (nth rows (dec (long (:v pos)))) col-idx))))))

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
                         result-vec (if (= 1 (count result)) (first result) (mapv first result))
                         pos (approx-position (map-indexed vector search) target <=)]
                     (if (val/err? pos) pos (nth result-vec (dec (long (:v pos))) val/ERR-NA)))
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
                         pos (approx-position (map-indexed vector search) target <=)]
                     (if (val/err? pos) pos (nth last-series (dec (long (:v pos))) val/ERR-NA))))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; INDEX

(f/register! "INDEX"
  ;; INDEX(array, row_num, [column_num])
  ;; row_num = 0 returns the whole column; column_num = 0 returns the whole row.
             (fn [args]
               (let [arr (nth args 0)
                     rows (area-rows arr)
                     ;; a whole column/row (B:B) is clipped to the used cells;
                     ;; INDEX counts over the reference, so a row past them
                     ;; is an empty cell, not #REF!
                     [nrows ncols] [(if (:ref-r1 arr) (inc (- (long (:ref-r1 arr)) (long (:ref-r0 arr)))) (count rows))
                                    (if (:ref-c1 arr) (inc (- (long (:ref-c1 arr)) (long (:ref-c0 arr)))) (count (first rows)))]
                     ;; a cell past the used ones is empty
                     cell-at (fn [r c] (get-in rows [r c] val/BLANK))
                     ;; one index into a one-row array counts along the row
                     ;; (INDEX({"a","b","c"},2) is "b"), as in Excel
                     along-row? (and (= 2 (count args)) (= 1 nrows) (> ncols 1))
                     given (long (f/num! (nth args 1)))
                     rn (if along-row? 1 given)
                     cn (cond along-row? given
                              (> (count args) 2) (long (f/num! (nth args 2)))
                              :else 1)]
                 (when (or (neg? rn) (> rn nrows)) (f/domain-error! :ref))
                 (when (or (neg? cn) (> cn ncols)) (f/domain-error! :ref))
                 ;; over a reference (an area with a :sheet), the result is a
                 ;; reference too: sub-areas keep their cells' coordinates, and
                 ;; a single cell carries :ref, so A1:INDEX(A1:A9,3) is A1:A3
                 (let [ref? (and (f/area? arr) (:sheet arr))
                       br (if ref? (long (:r0 arr)) 0)
                       bc (if ref? (long (:c0 arr)) 0)]
                   (cond
                     (and (zero? rn) (zero? cn)) arr
                     (zero? rn) ;; entire column cn
                     (cond-> {:t :area :r0 br :c0 (+ bc (dec cn)) :r1 (+ br (dec nrows)) :c1 (+ bc (dec cn))
                              :values (mapv (fn [row] [(nth row (dec cn))]) rows)}
                       ref? (assoc :sheet (:sheet arr)))
                     (zero? cn) ;; entire row rn
                     (cond-> {:t :area :r0 (+ br (dec rn)) :c0 bc :r1 (+ br (dec rn)) :c1 (+ bc (dec ncols))
                              :values [(nth rows (dec rn))]}
                       ref? (assoc :sheet (:sheet arr)))
                     :else
                     (cond-> (cell-at (dec rn) (dec cn))
                       ref? (assoc :ref {:sheet (:sheet arr) :row (+ br (dec rn)) :col (+ bc (dec cn))}))))))
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

(defn- offset-area
  "The area OFFSET gives for base `coords` and numbers `rows` `cols` `h` `w`
   (`h`/`w` nil: the base's)."
  [ctx coords rows cols h w]
  (let [h  (or h (inc (- (long (:r1 coords)) (long (:r0 coords)))))
        w  (or w (inc (- (long (:c1 coords)) (long (:c0 coords)))))
        r0 (+ (long (:r0 coords)) (long rows))
        c0 (+ (long (:c0 coords)) (long cols))
        r1 (+ r0 (dec (long h)))
        c1 (+ c0 (dec (long w)))]
    (if (or (neg? r0) (neg? c0) (< r1 r0) (< c1 c0))
      val/ERR-REF
      (if-let [resolve-area (:resolve-area ctx)]
        (resolve-area ctx {:sheet (:sheet coords) :r0 r0 :c0 c0 :r1 r1 :c1 c1})
        val/ERR-REF))))

(f/register! "OFFSET"
  ;; OFFSET(ref, rows, cols, [height], [width]) — shift the reference
  ;; by (rows, cols); optionally resize to (height, width). Height/width
  ;; default to the reference's own dimensions. Returns an area that
  ;; the evaluator then resolves into values. With an array of rows, cols,
  ;; heights or widths, an array of references (`f/refs?`), one per
  ;; element, broadcast as arrays are.
             (fn [ctx ast-args]
               (let [ref-ast (first ast-args)
                     ;; an omitted argument (OFFSET(A1,0,0,,3)) is its default
                     given?  #(and (> (count ast-args) %) (not= :missing (:op (nth ast-args %))))
                     arg-at  #(when (given? %) (eval1 ctx (nth ast-args %)))
                     ;; the base may be computed (OFFSET(INDIRECT(…),…),
                     ;; OFFSET(INDEX(…),…)): a result that is a reference
                     coords  (or (ast->ref-coords ref-ast)
                                 (let [v (eval1 ctx ref-ast)]
                                   (cond
                                     (and (f/area? v) (:sheet v))
                                     {:sheet (:sheet v) :r0 (:r0 v) :c0 (:c0 v) :r1 (:r1 v) :c1 (:c1 v)}
                                     (:ref v)
                                     (let [{:keys [sheet row col]} (:ref v)]
                                       {:sheet sheet :r0 row :c0 col :r1 row :c1 col}))))
                     ;; rows cols height width: numbers, nil when omitted, or arrays
                     [rows cols h w :as ns] [(or (arg-at 1) (val/number 0)) (or (arg-at 2) (val/number 0)) (arg-at 3) (arg-at 4)]
                     num (fn [v] (when (some? v) (long (f/num! v))))]
                 (cond
                   (not coords) val/ERR-VALUE
                   (some f/area? ns)
                   (let [arrays (filter f/area? ns)
                         nr (reduce max (map #(count (:values %)) arrays))
                         nc (reduce max (map #(count (first (:values %))) arrays))
                         at (fn [v r c]
                              (if (f/area? v)
                                (let [g (:values v) row (nth g (if (= 1 (count g)) 0 r) nil)]
                                  (nth row (if (= 1 (count row)) 0 c) nil))
                                v))]
                     {:t :refs
                      :values (mapv (fn [r]
                                      (mapv (fn [c]
                                              (let [[a b hh ww] (map #(at % r c) ns)]
                                                (if (and (some? a) (some? b) (or (nil? (nth ns 2)) (some? hh)) (or (nil? (nth ns 3)) (some? ww)))
                                                  (try (offset-area ctx coords (num a) (num b) (num hh) (num ww))
                                                       (catch #?(:clj Throwable :cljs :default) e
                                                         (or (some-> (ex-data e) :excel-error val/error) val/ERR-VALUE)))
                                                  val/ERR-NA)))
                                            (range nc)))
                                    (range nr))})
                   :else (offset-area ctx coords (num rows) (num cols) (num h) (num w)))))
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
