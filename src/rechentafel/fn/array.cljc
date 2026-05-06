(ns rechentafel.fn.array
  "Excel 365 dynamic-array functions and LAMBDA helpers.

  All functions in this module take and/or return :area values; they
  register :lift? false so the call dispatcher hands them areas
  directly instead of element-wise iterating.

  LAMBDA helpers (MAP, REDUCE, SCAN, BYROW, BYCOL, MAKEARRAY) are
  registered :lazy? so they receive the un-evaluated AST and the
  evaluator's lazy-ctx — they evaluate the array arg(s) themselves
  and apply the lambda value via `eval/call-lambda-with-values`."
  (:require [rechentafel.value :as val]
            [rechentafel.functions :as f]
            [rechentafel.rng :as rng]
            [rechentafel.eval :as e]))

;; ---------------------------------------------------------------------------
;; Helpers

(defn- num-arg [args i] (f/num! (nth args i)))

(defn- int-arg
  ([args i] (long (num-arg args i)))
  ([args i default]
   (if (>= (count args) (inc i))
     (long (num-arg args i))
     default)))

(defn- area-of
  "Coerce a value to a 2D vec-of-vec of cells.  Scalar → [[scalar]].
  Areas pass through (their :values vector). 3D areas become #VALUE!
  per Excel."
  [v]
  (cond
    (= :area (:t v)) (:values v)
    (= :area3d (:t v)) (f/domain-error! :value)
    :else [[v]]))

(defn- area-result
  "Build a `:t :area` value from a vec-of-vec of cells. Coerces both
  outer and inner sequences to vectors so downstream `get-in` works
  (lazy seqs are not associative)."
  [rows]
  (let [rows (if (seq rows) (mapv vec rows) [[val/BLANK]])
        nrow (count rows)
        ncol (count (first rows))]
    {:t :area :r0 0 :c0 0
     :r1 (dec nrow) :c1 (dec ncol)
     :values rows}))

;; ---------------------------------------------------------------------------
;; Shape constructors

(defn- gen-2d
  "Build a vec-of-vec by invoking `f` at each (r, c) for given dims."
  [rows cols f]
  (vec (for [r (range rows)]
         (vec (for [c (range cols)]
                (f r c))))))

(f/register! "SEQUENCE"
  ;; SEQUENCE(rows, [cols], [start], [step]) — produces a rows×cols
  ;; numeric area starting at `start` and stepping by `step` row-major.
             (fn [args]
               (let [rows  (int-arg args 0)
                     cols  (int-arg args 1 1)
                     start (if (>= (count args) 3) (num-arg args 2) 1.0)
                     step  (if (>= (count args) 4) (num-arg args 3) 1.0)]
                 (when (or (<= rows 0) (<= cols 0)) (f/domain-error! :value))
                 (area-result
                  (gen-2d rows cols
                          (fn [r c]
                            (val/number (+ start (* step (+ (* r cols) c)))))))))
             :arity [1 4])

(f/register! "RANDARRAY"
  ;; RANDARRAY([rows], [cols], [min], [max], [whole-number?])
             (fn [args]
               (let [rows  (int-arg args 0 1)
                     cols  (int-arg args 1 1)
                     mn    (if (>= (count args) 3) (num-arg args 2) 0.0)
                     mx    (if (>= (count args) 4) (num-arg args 3) 1.0)
                     int?  (and (>= (count args) 5)
                                (val/truthy? (nth args 4)))]
                 (when (or (<= rows 0) (<= cols 0)) (f/domain-error! :value))
                 (area-result
                  (gen-2d rows cols
                          (fn [_r _c]
                            (let [r (+ mn (* (- mx mn) (rng/next-double)))]
                              (val/number (if int? (Math/floor r) r))))))))
             :arity [0 5] :volatile? true)

(f/register! "MUNIT"
  ;; Identity matrix N×N.
             (fn [args]
               (let [n (int-arg args 0)]
                 (when (<= n 0) (f/domain-error! :value))
                 (area-result
                  (gen-2d n n (fn [r c] (val/number (if (= r c) 1.0 0.0)))))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Reshape / select

(f/register! "TRANSPOSE"
             (fn [args]
               (let [v (first args)
                     m (area-of v)
                     nrow (count m)
                     ncol (count (first m))]
                 (area-result
                  (vec (for [c (range ncol)]
                         (vec (for [r (range nrow)]
                                (get-in m [r c] val/BLANK))))))))
             :arity [1 1])

(defn- collect-int-args
  "Pull integer indices out of args[from..]. Areas expand."
  [args from]
  (let [tail (drop from args)]
    (mapcat (fn [v]
              (if (= :area (:t v))
                (for [row (:values v) cell row]
                  (long (f/num! cell)))
                [(long (f/num! v))]))
            tail)))

(f/register! "CHOOSEROWS"
             (fn [args]
               (let [m (area-of (first args))
                     nrow (count m)
                     idxs (collect-int-args args 1)]
                 (area-result
                  (vec (for [i idxs]
                         (let [r (if (neg? i) (+ nrow i) (dec i))]
                           (when (or (< r 0) (>= r nrow)) (f/domain-error! :value))
                           (nth m r)))))))
             :arity [2 nil])

(f/register! "CHOOSECOLS"
             (fn [args]
               (let [m (area-of (first args))
                     ncol (count (first m))
                     idxs (collect-int-args args 1)]
                 (area-result
                  (vec (for [row m]
                         (vec (for [i idxs]
                                (let [c (if (neg? i) (+ ncol i) (dec i))]
                                  (when (or (< c 0) (>= c ncol)) (f/domain-error! :value))
                                  (nth row c)))))))))
             :arity [2 nil])

(f/register! "DROP"
  ;; DROP(array, rows, [cols]) — strip leading/trailing rows/cols.
  ;; Negative drops from the end.
             (fn [args]
               (let [m  (area-of (first args))
                     nr (count m)
                     nc (count (first m))
                     dr (int-arg args 1 0)
                     dc (int-arg args 2 0)
                     [r0 r1] (if (neg? dr) [0 (+ nr dr)] [dr nr])
                     [c0 c1] (if (neg? dc) [0 (+ nc dc)] [dc nc])]
                 (when (or (>= r0 r1) (>= c0 c1)) (f/domain-error! :calc))
                 (area-result
                  (vec (for [r (range r0 r1)]
                         (vec (for [c (range c0 c1)] (get-in m [r c]))))))))
             :arity [2 3])

(f/register! "TAKE"
  ;; TAKE(array, rows, [cols]) — keep first/last N. Negative keeps from end.
             (fn [args]
               (let [m  (area-of (first args))
                     nr (count m)
                     nc (count (first m))
                     tr (int-arg args 1 nr)
                     tc (int-arg args 2 nc)
                     [r0 r1] (cond (zero? tr) [0 nr]
                                   (pos? tr)  [0 (min tr nr)]
                                   :else      [(max 0 (+ nr tr)) nr])
                     [c0 c1] (cond (zero? tc) [0 nc]
                                   (pos? tc)  [0 (min tc nc)]
                                   :else      [(max 0 (+ nc tc)) nc])]
                 (area-result
                  (vec (for [r (range r0 r1)]
                         (vec (for [c (range c0 c1)] (get-in m [r c]))))))))
             :arity [2 3])

(f/register! "EXPAND"
  ;; EXPAND(array, rows, [cols], [pad_with]) — pad with a value.
             (fn [args]
               (let [m   (area-of (first args))
                     nr  (count m)
                     nc  (count (first m))
                     tr  (int-arg args 1 nr)
                     tc  (int-arg args 2 nc)
                     pad (if (>= (count args) 4) (nth args 3) val/ERR-NA)]
                 (when (or (< tr nr) (< tc nc)) (f/domain-error! :value))
                 (area-result
                  (vec (for [r (range tr)]
                         (vec (for [c (range tc)]
                                (if (and (< r nr) (< c nc))
                                  (get-in m [r c])
                                  pad))))))))
             :arity [2 4])

(f/register! "HSTACK"
             (fn [args]
               (let [grids (map area-of args)
                     rows  (apply max (map count grids))]
                 (area-result
                  (vec (for [r (range rows)]
                         (vec (mapcat (fn [g]
                                        (let [row (get g r)]
                                          (or row (vec (repeat (count (first g)) val/ERR-NA)))))
                                      grids)))))))
             :arity [1 nil])

(f/register! "VSTACK"
             (fn [args]
               (let [grids (map area-of args)
                     ncol  (apply max (map (comp count first) grids))]
                 (area-result
                  (vec (mapcat (fn [g]
                                 (let [pad (- ncol (count (first g)))]
                                   (mapv (fn [row]
                                           (if (pos? pad)
                                             (into row (repeat pad val/ERR-NA))
                                             row))
                                         g)))
                               grids)))))
             :arity [1 nil])

(f/register! "TOROW"
  ;; TOROW(array, [ignore], [scan_by_column]) — flatten to a single row.
             (fn [args]
               (let [m       (area-of (first args))
                     ignore  (int-arg args 1 0)
                     by-col? (and (>= (count args) 3) (val/truthy? (nth args 2)))
                     cells   (if by-col?
                               (for [c (range (count (first m))), row m] (nth row c))
                               (for [row m, cell row] cell))
                     keep?   (case ignore
                               0 (constantly true)
                               1 (fn [v] (not (val/blank? v)))
                               2 (fn [v] (not (val/err? v)))
                               3 (fn [v] (and (not (val/blank? v)) (not (val/err? v))))
                               (constantly true))
                     kept (filter keep? cells)]
                 (area-result [(vec (or (seq kept) [val/BLANK]))])))
             :arity [1 3])

(f/register! "TOCOL"
             (fn [args]
               (let [m       (area-of (first args))
                     ignore  (int-arg args 1 0)
                     by-col? (and (>= (count args) 3) (val/truthy? (nth args 2)))
                     cells   (if by-col?
                               (for [c (range (count (first m))), row m] (nth row c))
                               (for [row m, cell row] cell))
                     keep?   (case ignore
                               0 (constantly true)
                               1 (fn [v] (not (val/blank? v)))
                               2 (fn [v] (not (val/err? v)))
                               3 (fn [v] (and (not (val/blank? v)) (not (val/err? v))))
                               (constantly true))
                     kept (filter keep? cells)]
                 (area-result (mapv vector (or (seq kept) [val/BLANK])))))
             :arity [1 3])

(f/register! "WRAPROWS"
  ;; WRAPROWS(vec, wrap_count, [pad]) — vec → 2D, row-major, padding.
             (fn [args]
               (let [v       (first args)
                     flat    (case (:t v)
                               :area (vec (for [row (:values v), cell row] cell))
                               [v])
                     wrap    (int-arg args 1)
                     pad     (if (>= (count args) 3) (nth args 2) val/ERR-NA)
                     n       (count flat)
                     nrows   (long (Math/ceil (/ (double n) (double wrap))))]
                 (when (<= wrap 0) (f/domain-error! :value))
                 (area-result
                  (vec (for [r (range nrows)]
                         (vec (for [c (range wrap)]
                                (let [i (+ (* r wrap) c)]
                                  (if (< i n) (nth flat i) pad)))))))))
             :arity [2 3])

(f/register! "WRAPCOLS"
             (fn [args]
               (let [v       (first args)
                     flat    (case (:t v)
                               :area (vec (for [row (:values v), cell row] cell))
                               [v])
                     wrap    (int-arg args 1)
                     pad     (if (>= (count args) 3) (nth args 2) val/ERR-NA)
                     n       (count flat)
                     ncols   (long (Math/ceil (/ (double n) (double wrap))))]
                 (when (<= wrap 0) (f/domain-error! :value))
                 (area-result
                  (vec (for [r (range wrap)]
                         (vec (for [c (range ncols)]
                                (let [i (+ (* c wrap) r)]
                                  (if (< i n) (nth flat i) pad)))))))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; Filtering / sorting

(f/register! "UNIQUE"
  ;; UNIQUE(array, [by_col?], [exactly_once?])
             (fn [args]
               (let [m       (area-of (first args))
                     by-col? (and (>= (count args) 2) (val/truthy? (nth args 1)))
                     once?   (and (>= (count args) 3) (val/truthy? (nth args 2)))
                     rows    (if by-col?
                               (mapv (fn [c] (mapv #(nth % c) m))
                                     (range (count (first m))))
                               m)
                     freq    (frequencies rows)
                     kept    (if once?
                               (filterv #(= 1 (get freq %)) (distinct rows))
                               (vec (distinct rows)))
                     out     (if by-col?
                               (mapv (fn [c] (mapv #(nth % c) kept))
                                     (range (count (first (or (seq kept) [[val/BLANK]])))))
                               kept)]
                 (area-result (or (seq out) [[val/BLANK]]))))
             :arity [1 3])

(f/register! "SORT"
  ;; SORT(array, [sort_index=1], [sort_order=1], [by_col?=false])
             (fn [args]
               (let [m       (area-of (first args))
                     idx     (int-arg args 1 1)
                     asc?    (or (< (count args) 3)
                                 (>= (long (f/num! (nth args 2))) 0))
                     by-col? (and (>= (count args) 4) (val/truthy? (nth args 3)))
                     cmp     (fn [a b]
                               (let [c (compare (:v a) (:v b))]
                                 (if asc? c (- c))))]
                 (if by-col?
                   (let [cols (mapv (fn [c] (mapv #(nth % c) m))
                                    (range (count (first m))))
                         sorted (sort-by #(nth % (dec idx)) cmp cols)]
                     (area-result
                      (mapv (fn [r] (mapv #(nth % r) sorted))
                            (range (count m)))))
                   (area-result (vec (sort-by #(nth % (dec idx)) cmp m))))))
             :arity [1 4])

(f/register! "SORTBY"
  ;; SORTBY(array, by_array1, [order1], by_array2, [order2], ...)
             (fn [args]
               (let [m      (area-of (first args))
                     rest-args (vec (drop 1 args))
          ;; pair up by-array + optional order
                     pairs  (loop [acc [] xs rest-args]
                              (if (empty? xs) acc
                                  (let [ba    (first xs)
                                        order (if (and (>= (count xs) 2)
                                                       (val/num? (second xs)))
                                                (long (:v (second xs)))
                                                1)
                                        rest' (if (and (>= (count xs) 2)
                                                       (val/num? (second xs)))
                                                (drop 2 xs) (drop 1 xs))]
                                    (recur (conj acc [ba order]) rest'))))
                     decorated (map-indexed
                                (fn [i row]
                                  [(mapv (fn [[ba _]]
                                           (let [bm (area-of ba)]
                                             (get-in bm [i 0])))
                                         pairs)
                                   row])
                                m)
                     orders (mapv second pairs)
                     cmp    (fn [[ka _] [kb _]]
                              (loop [i 0]
                                (if (>= i (count ka)) 0
                                    (let [c (compare (:v (nth ka i)) (:v (nth kb i)))
                                          c (if (neg? (long (nth orders i))) (- c) c)]
                                      (if (zero? c) (recur (inc i)) c)))))
                     sorted (sort cmp decorated)]
                 (area-result (mapv second sorted))))
             :arity [2 nil])

(f/register! "FILTER"
  ;; FILTER(array, include, [if_empty])
             (fn [args]
               (let [m         (area-of (first args))
                     include   (area-of (nth args 1))
                     if-empty  (if (>= (count args) 3) (nth args 2) val/ERR-CALC)
                     horizontal? (and (= 1 (count m)) (= (count (first include))
                                                         (count (first m))))
          ;; Column-of-flags: keep matching rows; row-of-flags: keep matching cols
                     flag-of   (if horizontal?
                                 (fn [c] (val/truthy? (get-in include [0 c])))
                                 (fn [r] (val/truthy? (get-in include [r 0]))))
                     kept (if horizontal?
                            (let [cols (filter flag-of (range (count (first m))))]
                              (when (seq cols)
                                [(mapv #(nth (first m) %) cols)]))
                            (let [rows (filter flag-of (range (count m)))]
                              (when (seq rows)
                                (mapv #(nth m %) rows))))]
                 (if (seq kept)
                   (area-result (vec kept))
                   if-empty)))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; LAMBDA helpers — registered :lazy? because they iterate the lambda
;; per cell. Args evaluated via (:eval ctx); lambda invocation goes
;; through eval/call-lambda-with-values.

(defn- eval-arg [ctx ast]
  ((:eval ctx) ctx ast))

(defn- as-lambda [v]
  (when (val/lambda? v) v))

(defn- scalarize
  "Coerce a lambda's per-cell result to a scalar. A 1x1 area unwraps
  to its single value (Excel behaviour: scalar-context broadcasting
  collapses 1x1 results); a larger area is the 'nested array' error
  case → #CALC!. Other values pass through untouched."
  [v]
  (case (:t v)
    :area (if (and (= (long (:r0 v)) (long (:r1 v)))
                   (= (long (:c0 v)) (long (:c1 v))))
            (get-in (:values v) [0 0] val/BLANK)
            val/ERR-CALC)
    :area3d val/ERR-CALC
    v))

(f/register! "MAP"
  ;; MAP(array1, [array2, ...], lambda)
             (fn [ctx ast-args]
               (let [arrays  (vec (map #(eval-arg ctx %) (butlast ast-args)))
                     lam     (as-lambda (eval-arg ctx (last ast-args)))]
                 (cond
                   (nil? lam) val/ERR-VALUE
                   :else
                   (let [grids (map area-of arrays)
                         nrow  (apply max (map count grids))
                         ncol  (apply max (map (comp count first) grids))]
                     (area-result
                      (vec (for [r (range nrow)]
                             (vec (for [c (range ncol)]
                                    (let [args (mapv (fn [g] (get-in g [r c] val/BLANK))
                                                     grids)]
                                      (scalarize
                                       (e/call-lambda-with-values (:wb ctx) lam args))))))))))))
             :arity [2 nil] :lazy? true)

(f/register! "REDUCE"
  ;; REDUCE([initial], array, lambda(acc, val)) — row-major fold.
             (fn [ctx ast-args]
               (let [init (eval-arg ctx (nth ast-args 0))
                     a    (eval-arg ctx (nth ast-args 1))
                     lam  (as-lambda (eval-arg ctx (nth ast-args 2)))]
                 (cond
                   (nil? lam) val/ERR-VALUE
                   :else
                   (let [m (area-of a)
                         cells (for [row m, cell row] cell)]
                     (reduce (fn [acc v]
                               (e/call-lambda-with-values (:wb ctx) lam [acc v]))
                             (if (val/omitted? init) val/BLANK init)
                             cells)))))
             :arity [3 3] :lazy? true)

(f/register! "SCAN"
             (fn [ctx ast-args]
               (let [init (eval-arg ctx (nth ast-args 0))
                     a    (eval-arg ctx (nth ast-args 1))
                     lam  (as-lambda (eval-arg ctx (nth ast-args 2)))]
                 (cond
                   (nil? lam) val/ERR-VALUE
                   :else
                   (let [m (area-of a)
                         nrow (count m)
                         ncol (count (first m))
                         acc  (volatile! (if (val/omitted? init) val/BLANK init))]
                     (area-result
                      (vec (for [r (range nrow)]
                             (vec (for [c (range ncol)]
                                    (let [v (get-in m [r c])
                                          result (scalarize
                                                  (e/call-lambda-with-values
                                                   (:wb ctx) lam [@acc v]))]
                                      (vreset! acc result)
                                      result))))))))))
             :arity [3 3] :lazy? true)

(f/register! "BYROW"
  ;; BYROW(array, lambda(row)) — body sees a 1xN sub-area per row
             (fn [ctx ast-args]
               (let [a   (eval-arg ctx (nth ast-args 0))
                     lam (as-lambda (eval-arg ctx (nth ast-args 1)))]
                 (cond
                   (nil? lam) val/ERR-VALUE
                   :else
                   (let [m (area-of a)]
                     (area-result
                      (mapv (fn [row]
                              [(scalarize
                                (e/call-lambda-with-values
                                 (:wb ctx) lam
                                 [(area-result [row])]))])
                            m))))))
             :arity [2 2] :lazy? true)

(f/register! "BYCOL"
  ;; BYCOL(array, lambda(col)) — body sees an Nx1 sub-area per column
             (fn [ctx ast-args]
               (let [a   (eval-arg ctx (nth ast-args 0))
                     lam (as-lambda (eval-arg ctx (nth ast-args 1)))]
                 (cond
                   (nil? lam) val/ERR-VALUE
                   :else
                   (let [m (area-of a)
                         nrow (count m)
                         ncol (count (first m))]
                     (area-result
                      [(vec (for [c (range ncol)]
                              (let [col (mapv (fn [r] [(nth (nth m r) c)])
                                              (range nrow))]
                                (scalarize
                                 (e/call-lambda-with-values
                                  (:wb ctx) lam [(area-result col)])))))])))))
             :arity [2 2] :lazy? true)

(f/register! "MAKEARRAY"
             (fn [ctx ast-args]
               (let [rows (long (f/num! (eval-arg ctx (nth ast-args 0))))
                     cols (long (f/num! (eval-arg ctx (nth ast-args 1))))
                     lam  (as-lambda (eval-arg ctx (nth ast-args 2)))]
                 (cond
                   (nil? lam) val/ERR-VALUE
                   (or (<= rows 0) (<= cols 0)) val/ERR-VALUE
                   :else
                   (area-result
                    (vec (for [r (range rows)]
                           (vec (for [c (range cols)]
                                  (scalarize
                                   (e/call-lambda-with-values
                                    (:wb ctx) lam
                                    [(val/number (inc r)) (val/number (inc c))]))))))))))
             :arity [3 3] :lazy? true)
