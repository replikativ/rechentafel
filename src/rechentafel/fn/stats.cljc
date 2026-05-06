(ns rechentafel.fn.stats
  "Statistics functions (POI category: statistics — 99 fns).

  Broken into sections by shape:

    - Aggregates (AVERAGE, MAX, MIN, DEVSQ, GEOMEAN, HARMEAN, KURT, SKEW,
      STDEV family, VAR family, MEDIAN, MODE)
    - Counters (COUNT, COUNTA, COUNTBLANK, and *IF/*IFS family)
    - Percentile / rank (PERCENTILE, PERCENTRANK, QUARTILE, LARGE, SMALL,
      RANK, TRIMMEAN)
    - Regression (SLOPE, INTERCEPT, FORECAST, TREND, CORREL, COVAR,
      STEYX, RSQ)
    - Distributions (NORM.DIST family, POISSON, T.DIST family, BINOMDIST,
      EXPONDIST, LOGNORMDIST, WEIBULL, GAMMA family, etc.)
    - Transforms (STANDARDIZE, FISHER, FISHERINV)

  Distribution functions that POI itself marks NotImplementedFunction
  without a viable pure-cljc implementation are registered as #N/A
  stubs so formulas referencing them don't blow up parsing. The rest
  are fully implemented using standard numerical-analysis recipes
  (Abramowitz & Stegun, Lanczos, regularised incomplete beta, etc.)."
  (:require [clojure.string :as str]
            [rechentafel.platform :as p]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Scalar walking — specialised aggregators for POI's MINA/MAXA/VARA/STDEVA
;; style where every value contributes (booleans as 0/1, non-numeric strings
;; as 0 instead of being skipped).

(defn- collect-aggregate-a
  "AVERAGEA / MAXA / MINA / STDEVA / STDEVPA / VARA / VARPA semantics:
  numbers contribute as-is, booleans as 0/1, empty strings in cells skip,
  other strings count as 0, blanks in AREAS skip, blanks top-level count
  as 0."
  [args]
  (let [t (volatile! (transient []))]
    (f/each-scalar
     args
     (fn [v in-area?]
       (case (:t v)
         :num   (vswap! t conj! (double (:v v)))
         :bool  (vswap! t conj! (if (:v v) 1.0 0.0))
         :str   (when-not (and in-area? (= "" (:v v)))
                  (vswap! t conj! 0.0))
         :err   (f/domain-error! (:v v))
         :blank (when-not in-area? (vswap! t conj! 0.0))
         nil)))
    (persistent! @t)))

(defn- count-all-a
  "COUNTA-style counter but counting via the aggregate-a rules above —
  used by MAXA/MINA to know whether anything was seen."
  [xs] (count xs))

;; ---------------------------------------------------------------------------
;; Basic aggregates

(f/register! "AVERAGE"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)]
                 (if (empty? xs) val/ERR-DIV0
                     (val/number (/ (reduce + 0.0 xs) (double (count xs)))))))
             :arity [1 30])

(f/register! "AVERAGEA"
             (fn [args]
               (let [xs (collect-aggregate-a args)]
                 (if (empty? xs) val/ERR-DIV0
                     (val/number (/ (reduce + 0.0 xs) (double (count xs)))))))
             :arity [1 30])

(f/register! "COUNT"
             (fn [args] (val/number (double (f/count-numeric args))))
             :arity [0 30])

(f/register! "COUNTA"
             (fn [args] (val/number (double (f/count-all args))))
             :arity [0 30])

(f/register! "COUNTBLANK"
             (fn [args]
               (let [acc (volatile! 0)]
                 (f/each-scalar
                  args
                  (fn [v _]
                    (when (or (val/blank? v)
                              (and (val/str? v) (= "" (:v v))))
                      (vswap! acc inc))))
                 (val/number (double @acc))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; MAX / MIN / MAXA / MINA

(defn- max-of [args]
  (let [xs (f/collect-finite-numerics args)]
    (if (empty? xs) 0.0 (apply max xs))))

(defn- min-of [args]
  (let [xs (f/collect-finite-numerics args)]
    (if (empty? xs) 0.0 (apply min xs))))

(f/register! "MAX" (fn [args] (val/number (max-of args))) :arity [1 30])
(f/register! "MIN" (fn [args] (val/number (min-of args))) :arity [1 30])

(f/register! "MAXA"
             (fn [args]
               (let [xs (collect-aggregate-a args)]
                 (val/number (if (empty? xs) 0.0 (apply max xs)))))
             :arity [1 30])

(f/register! "MINA"
             (fn [args]
               (let [xs (collect-aggregate-a args)]
                 (val/number (if (empty? xs) 0.0 (apply min xs)))))
             :arity [1 30])

;; ---------------------------------------------------------------------------
;; Variance / stdev family

(defn- sum-sq-dev ^double [xs]
  (let [n (count xs)]
    (if (zero? n) 0.0
        (let [mean (/ (reduce + 0.0 xs) (double n))]
          (reduce + 0.0 (map #(let [d (- % mean)] (* d d)) xs))))))

(defn- variance ^double [xs sample?]
  (let [n (count xs)]
    (if (or (zero? n) (and sample? (<= n 1)))
      (f/domain-error! :div0)
      (/ (sum-sq-dev xs) (double (if sample? (dec n) n))))))

(f/register! "DEVSQ"
             (fn [args] (val/number (sum-sq-dev (f/collect-finite-numerics args))))
             :arity [1 30])

(f/register! "VAR"
             (fn [args] (val/number (variance (f/collect-finite-numerics args) true)))
             :arity [1 30])

(f/register! "VAR.S"
             (fn [args] (val/number (variance (f/collect-finite-numerics args) true)))
             :arity [1 254])

(f/register! "VARP"
             (fn [args] (val/number (variance (f/collect-finite-numerics args) false)))
             :arity [1 30])

(f/register! "VAR.P"
             (fn [args] (val/number (variance (f/collect-finite-numerics args) false)))
             :arity [1 254])

(f/register! "VARA"
             (fn [args] (val/number (variance (collect-aggregate-a args) true)))
             :arity [1 30])

(f/register! "VARPA"
             (fn [args] (val/number (variance (collect-aggregate-a args) false)))
             :arity [1 30])

(f/register! "STDEV"
             (fn [args] (val/number (Math/sqrt (variance (f/collect-finite-numerics args) true))))
             :arity [1 30])

(f/register! "STDEV.S"
             (fn [args] (val/number (Math/sqrt (variance (f/collect-finite-numerics args) true))))
             :arity [1 254])

(f/register! "STDEVP"
             (fn [args] (val/number (Math/sqrt (variance (f/collect-finite-numerics args) false))))
             :arity [1 30])

(f/register! "STDEV.P"
             (fn [args] (val/number (Math/sqrt (variance (f/collect-finite-numerics args) false))))
             :arity [1 254])

(f/register! "STDEVA"
             (fn [args] (val/number (Math/sqrt (variance (collect-aggregate-a args) true))))
             :arity [1 30])

(f/register! "STDEVPA"
             (fn [args] (val/number (Math/sqrt (variance (collect-aggregate-a args) false))))
             :arity [1 30])

;; ---------------------------------------------------------------------------
;; MEDIAN / MODE

(f/register! "MEDIAN"
             (fn [args]
               (let [xs (sort (f/collect-finite-numerics args))
                     n  (count xs)]
                 (cond
                   (zero? n) val/ERR-NUM
                   (odd? n)  (val/number (nth xs (quot n 2)))
                   :else     (val/number (/ (+ (nth xs (dec (quot n 2)))
                                               (nth xs (quot n 2)))
                                            2.0)))))
             :arity [1 30])

(f/register! "MODE"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)
                     freqs (frequencies xs)
                     best (apply max (map second freqs))]
                 (if (<= best 1) val/ERR-NA
          ;; smallest x with max frequency in original-first order
                     (val/number (first (filter #(= best (get freqs %)) xs))))))
             :arity [1 30])

(f/register! "MODE.SNGL"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)
                     freqs (frequencies xs)
                     best (apply max (map second freqs))]
                 (if (<= best 1) val/ERR-NA
                     (val/number (first (filter #(= best (get freqs %)) xs))))))
             :arity [1 254])

;; ---------------------------------------------------------------------------
;; LARGE / SMALL

(f/register! "LARGE"
             (fn [args]
               (let [xs (vec (sort > (f/collect-finite-numerics args)))
                     k  (long (f/num! (nth args 1)))]
                 (if (or (<= k 0) (> k (count xs)))
                   val/ERR-NUM
                   (val/number (nth xs (dec k))))))
             :arity [2 2])

(f/register! "SMALL"
             (fn [args]
               (let [xs (vec (sort (f/collect-finite-numerics args)))
                     k  (long (f/num! (nth args 1)))]
                 (if (or (<= k 0) (> k (count xs)))
                   val/ERR-NUM
                   (val/number (nth xs (dec k))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; PERCENTILE / QUARTILE / PERCENTRANK

(defn- percentile-of
  "Linear-interpolation percentile, matching Excel inclusive (type-7)
  semantics used by PERCENTILE/QUARTILE (and their .INC variants)."
  ^double [xs ^double p]
  (let [xs (vec (sort xs))
        n  (count xs)]
    (when (zero? n) (f/domain-error! :num))
    (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
    (let [idx (* p (dec n))
          lo  (long (Math/floor idx))
          hi  (long (Math/ceil idx))
          frac (- idx lo)]
      (if (= lo hi)
        (double (nth xs lo))
        (+ (double (nth xs lo))
           (* frac (- (double (nth xs hi)) (double (nth xs lo)))))))))

(defn- percentile-exc
  "Exclusive-interpolation percentile (PERCENTILE.EXC). Index = p*(n+1),
  valid for 1/(n+1) ≤ p ≤ n/(n+1)."
  ^double [xs ^double p]
  (let [xs (vec (sort xs))
        n  (count xs)]
    (when (zero? n) (f/domain-error! :num))
    (let [lo-bound (/ 1.0 (double (inc n)))
          hi-bound (/ (double n) (double (inc n)))]
      (when (or (< p lo-bound) (> p hi-bound))
        (f/domain-error! :num)))
    (let [idx (* p (inc n))
          lo  (long (Math/floor idx))
          hi  (long (Math/ceil idx))
          frac (- idx lo)]
      (if (= lo hi)
        (double (nth xs (dec lo)))
        (+ (double (nth xs (dec lo)))
           (* frac (- (double (nth xs (dec hi)))
                      (double (nth xs (dec lo))))))))))

(f/register! "PERCENTILE"
             (fn [args]
               (val/number (percentile-of (f/collect-finite-numerics [(nth args 0)])
                                          (f/num! (nth args 1)))))
             :arity [2 2])

(f/register! "PERCENTILE.INC"
             (fn [args]
               (val/number (percentile-of (f/collect-finite-numerics [(nth args 0)])
                                          (f/num! (nth args 1)))))
             :arity [2 2])

(f/register! "PERCENTILE.EXC"
             (fn [args]
               (val/number (percentile-exc (f/collect-finite-numerics [(nth args 0)])
                                           (f/num! (nth args 1)))))
             :arity [2 2])

(defn- quartile-p [^long q]
  (case q 0 0.0 1 0.25 2 0.5 3 0.75 4 1.0 (f/domain-error! :num)))

(f/register! "QUARTILE"
             (fn [args]
               (val/number (percentile-of (f/collect-finite-numerics [(nth args 0)])
                                          (quartile-p (long (f/num! (nth args 1)))))))
             :arity [2 2])

(f/register! "QUARTILE.INC"
             (fn [args]
               (val/number (percentile-of (f/collect-finite-numerics [(nth args 0)])
                                          (quartile-p (long (f/num! (nth args 1)))))))
             :arity [2 2])

(f/register! "QUARTILE.EXC"
             (fn [args]
               (let [q (long (f/num! (nth args 1)))]
                 (when-not (<= 1 q 3) (f/domain-error! :num))
                 (val/number (percentile-exc (f/collect-finite-numerics [(nth args 0)])
                                             (case q 1 0.25 2 0.5 3 0.75)))))
             :arity [2 2])

(defn- round-signif ^double [^double x ^long digits]
  ;; Matches POI's `PercentRank.round`: round HALF_UP to digits+3 places, then
  ;; truncate (DOWN) to `digits` places. This two-stage rule reproduces the
  ;; inconsistent rounding observed in Excel for PERCENTRANK results.
  (let [p1 (Math/pow 10.0 (+ digits 3))
        stage1 (/ (Math/round (* x p1)) p1)
        p2 (Math/pow 10.0 digits)]
    (if (neg? stage1)
      (/ (Math/ceil (* stage1 p2)) p2)
      (/ (Math/floor (* stage1 p2)) p2))))

(defn- percentrank-inc ^double [xs ^double x ^long digits]
  (let [xs (vec (sort xs))
        n  (count xs)]
    (when (zero? n) (f/domain-error! :num))
    (when (or (< x (first xs)) (> x (last xs))) (f/domain-error! :na))
    ;; Exact match → rank/(n-1). Otherwise linear interp between neighbours.
    (let [idx (loop [i 0]
                (cond
                  (= i n)                nil
                  (= x (double (nth xs i))) i
                  :else                  (recur (inc i))))]
      (if idx
        (round-signif (/ (double idx) (double (dec n))) digits)
        (let [[lo hi] (loop [i 0]
                        (cond
                          (>= i (dec n)) [(dec n) (dec n)]
                          (and (< (double (nth xs i)) x)
                               (< x (double (nth xs (inc i)))))
                          [i (inc i)]
                          :else (recur (inc i))))
              a  (double (nth xs lo))
              b  (double (nth xs hi))
              frac (if (= lo hi) 0.0 (/ (- x a) (- b a)))
              rank (+ (double lo) frac)]
          (round-signif (/ rank (double (dec n))) digits))))))

(f/register! "PERCENTRANK"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     x  (f/num! (nth args 1))
                     dig (if (> (count args) 2) (long (f/num! (nth args 2))) 3)]
                 (val/number (percentrank-inc xs x dig))))
             :arity [2 3])

(f/register! "PERCENTRANK.INC"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     x  (f/num! (nth args 1))
                     dig (if (> (count args) 2) (long (f/num! (nth args 2))) 3)]
                 (val/number (percentrank-inc xs x dig))))
             :arity [2 3])

(f/register! "PERCENTRANK.EXC"
             (fn [args]
               (let [xs (vec (sort (f/collect-finite-numerics [(nth args 0)])))
                     x  (f/num! (nth args 1))
                     dig (if (> (count args) 2) (long (f/num! (nth args 2))) 3)
                     n  (count xs)]
                 (when (zero? n) (f/domain-error! :num))
                 (when (or (< x (first xs)) (> x (last xs))) (f/domain-error! :na))
                 (let [[lo hi] (loop [i 0]
                                 (cond
                                   (>= i (dec n)) [(dec n) (dec n)]
                                   (= (double (nth xs i)) x) [i i]
                                   (and (< (double (nth xs i)) x)
                                        (< x (double (nth xs (inc i)))))
                                   [i (inc i)]
                                   :else (recur (inc i))))
                       a  (double (nth xs lo))
                       b  (double (nth xs hi))
                       frac (if (= lo hi) 0.0 (/ (- x a) (- b a)))
                       rank (+ (inc (double lo)) frac)]
                   (val/number (round-signif (/ rank (double (inc n))) dig)))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; RANK

(defn- do-rank [x xs order]
  (let [desc? (or (nil? order) (zero? (long order)))
        sorted (vec (if desc? (sort > xs) (sort < xs)))
        idx (loop [i 0]
              (cond
                (= i (count sorted)) -1
                (== (double x) (double (nth sorted i))) i
                :else (recur (inc i))))]
    (if (neg? idx) val/ERR-NA (val/number (double (inc idx))))))

(f/register! "RANK"
             (fn [args]
               (do-rank (f/num! (nth args 0))
                        (f/collect-finite-numerics [(nth args 1)])
                        (when (> (count args) 2) (f/num! (nth args 2)))))
             :arity [2 3])

(f/register! "RANK.EQ"
             (fn [args]
               (do-rank (f/num! (nth args 0))
                        (f/collect-finite-numerics [(nth args 1)])
                        (when (> (count args) 2) (f/num! (nth args 2)))))
             :arity [2 3])

(f/register! "RANK.AVG"
             (fn [args]
               (let [x (f/num! (nth args 0))
                     xs (f/collect-finite-numerics [(nth args 1)])
                     desc? (or (<= (count args) 2) (zero? (long (f/num! (nth args 2)))))
                     sorted (vec (if desc? (sort > xs) (sort < xs)))
                     positions (keep-indexed (fn [i v] (when (== (double v) x) (inc i))) sorted)]
                 (if (empty? positions)
                   val/ERR-NA
                   (val/number (/ (reduce + 0.0 positions) (double (count positions)))))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; GEOMEAN / HARMEAN / TRIMMEAN

(f/register! "GEOMEAN"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)]
                 (if (empty? xs) val/ERR-NUM
                     (do
                       (when (some #(<= (double %) 0.0) xs)
                         (f/domain-error! :num))
                       (val/number (Math/pow (reduce * 1.0 xs)
                                             (/ 1.0 (double (count xs)))))))))
             :arity [1 30])

(f/register! "HARMEAN"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)]
                 (if (empty? xs) val/ERR-NUM
                     (do
                       (when (some #(<= (double %) 0.0) xs)
                         (f/domain-error! :num))
                       (val/number
                        (/ (double (count xs))
                           (reduce + 0.0 (map #(/ 1.0 (double %)) xs))))))))
             :arity [1 30])

(f/register! "TRIMMEAN"
             (fn [args]
               (let [xs (vec (sort (f/collect-finite-numerics [(nth args 0)])))
                     p  (f/num! (nth args 1))
                     n  (count xs)]
                 (when (or (< p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (zero? n) (f/domain-error! :num))
                 (let [trim (long (Math/floor (* (/ p 2.0) (double n))))
                       kept (subvec xs trim (- n trim))]
                   (val/number (/ (reduce + 0.0 kept)
                                  (double (count kept)))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; KURT / SKEW / SKEW.P

(defn- central-moment ^double [xs ^long k]
  (let [n (count xs)
        mean (/ (reduce + 0.0 xs) (double n))]
    (/ (reduce + 0.0 (map #(Math/pow (- % mean) k) xs))
       (double n))))

(f/register! "SKEW"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)
                     n  (count xs)]
                 (when (< n 3) (f/domain-error! :div0))
                 (let [mean (/ (reduce + 0.0 xs) (double n))
                       s (Math/sqrt (variance xs true))]
                   (when (zero? s) (f/domain-error! :div0))
                   (val/number
                    (* (/ (double n) (double (* (dec n) (- n 2))))
                       (reduce + 0.0 (map #(let [z (/ (- % mean) s)] (* z z z)) xs)))))))
             :arity [1 30])

(f/register! "SKEW.P"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)
                     n  (count xs)]
                 (when (< n 3) (f/domain-error! :div0))
                 (let [m2 (central-moment xs 2)
                       m3 (central-moment xs 3)]
                   (when (zero? m2) (f/domain-error! :div0))
                   (val/number (/ m3 (Math/pow m2 1.5))))))
             :arity [1 30])

(f/register! "KURT"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)
                     n  (count xs)]
                 (when (< n 4) (f/domain-error! :div0))
                 (let [mean (/ (reduce + 0.0 xs) (double n))
                       s (Math/sqrt (variance xs true))]
                   (when (zero? s) (f/domain-error! :div0))
                   (val/number
                    (- (* (/ (* (double n) (inc n))
                             (double (* (dec n) (- n 2) (- n 3))))
                          (reduce + 0.0
                                  (map #(let [z (/ (- % mean) s)] (* z z z z)) xs)))
                       (/ (* 3.0 (dec n) (dec n))
                          (double (* (- n 2) (- n 3)))))))))
             :arity [1 30])

;; ---------------------------------------------------------------------------
;; Pairwise regression helpers
;;
;; POI's Correl / Covar / Slope / Intercept all accept two arrays and
;; zip them element-wise; lengths must match, else #N/A.

(defn- pair-data
  "Return [xs ys] as double vectors; lengths must match (else #N/A)."
  [x-arg y-arg]
  (let [xs (f/collect-finite-numerics [x-arg])
        ys (f/collect-finite-numerics [y-arg])]
    (when (not= (count xs) (count ys)) (f/domain-error! :na))
    (when (zero? (count xs)) (f/domain-error! :div0))
    [xs ys]))

(defn- covariance ^double [xs ys sample?]
  (let [n (count xs)
        mx (/ (reduce + 0.0 xs) (double n))
        my (/ (reduce + 0.0 ys) (double n))
        sxy (reduce + 0.0 (map (fn [a b] (* (- a mx) (- b my))) xs ys))]
    (when (and sample? (<= n 1)) (f/domain-error! :div0))
    (/ sxy (double (if sample? (dec n) n)))))

(defn- correlation ^double [xs ys]
  (let [n (count xs)
        mx (/ (reduce + 0.0 xs) (double n))
        my (/ (reduce + 0.0 ys) (double n))
        sxy (reduce + 0.0 (map (fn [a b] (* (- a mx) (- b my))) xs ys))
        sxx (reduce + 0.0 (map (fn [a] (let [d (- a mx)] (* d d))) xs))
        syy (reduce + 0.0 (map (fn [b] (let [d (- b my)] (* d d))) ys))]
    (when (or (zero? sxx) (zero? syy)) (f/domain-error! :div0))
    (/ sxy (Math/sqrt (* sxx syy)))))

(f/register! "CORREL"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 0) (nth args 1))]
                 (val/number (correlation xs ys))))
             :arity [2 2])

(f/register! "PEARSON"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 0) (nth args 1))]
                 (val/number (correlation xs ys))))
             :arity [2 2])

(f/register! "RSQ"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 0) (nth args 1))
                     r (correlation xs ys)]
                 (val/number (* r r))))
             :arity [2 2])

(f/register! "COVAR"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 0) (nth args 1))]
                 (val/number (covariance xs ys false))))
             :arity [2 2])

(f/register! "COVARIANCE.P"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 0) (nth args 1))]
                 (val/number (covariance xs ys false))))
             :arity [2 2])

(f/register! "COVARIANCE.S"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 0) (nth args 1))]
                 (val/number (covariance xs ys true))))
             :arity [2 2])

(defn- slope-intercept [xs ys]
  (let [n (count xs)
        mx (/ (reduce + 0.0 xs) (double n))
        my (/ (reduce + 0.0 ys) (double n))
        sxy (reduce + 0.0 (map (fn [a b] (* (- a mx) (- b my))) xs ys))
        sxx (reduce + 0.0 (map (fn [a] (let [d (- a mx)] (* d d))) xs))]
    (when (zero? sxx) (f/domain-error! :div0))
    [(/ sxy sxx) (- my (* (/ sxy sxx) mx))]))

(f/register! "SLOPE"
  ;; Excel: SLOPE(known_ys, known_xs)
             (fn [args]
               (let [[xs ys] (pair-data (nth args 1) (nth args 0))
                     [m _]   (slope-intercept xs ys)]
                 (val/number m)))
             :arity [2 2])

(f/register! "INTERCEPT"
             (fn [args]
               (let [[xs ys] (pair-data (nth args 1) (nth args 0))
                     [_ b]   (slope-intercept xs ys)]
                 (val/number b)))
             :arity [2 2])

(f/register! "FORECAST"
             (fn [args]
               (let [x (f/num! (nth args 0))
          ;; Note Excel arg order: FORECAST(x, known_ys, known_xs)
                     [xs ys] (pair-data (nth args 2) (nth args 1))
                     [m b]   (slope-intercept xs ys)]
                 (val/number (+ b (* m x)))))
             :arity [3 3])

(f/register! "FORECAST.LINEAR"
             (fn [args]
               (let [x (f/num! (nth args 0))
                     [xs ys] (pair-data (nth args 2) (nth args 1))
                     [m b]   (slope-intercept xs ys)]
                 (val/number (+ b (* m x)))))
             :arity [3 3])

(f/register! "STEYX"
  ;; Excel: STEYX(known_ys, known_xs)
             (fn [args]
               (let [[xs ys] (pair-data (nth args 1) (nth args 0))
                     n  (count xs)]
                 (when (< n 3) (f/domain-error! :div0))
                 (let [mx (/ (reduce + 0.0 xs) (double n))
                       my (/ (reduce + 0.0 ys) (double n))
                       sxy (reduce + 0.0 (map (fn [a b] (* (- a mx) (- b my))) xs ys))
                       sxx (reduce + 0.0 (map (fn [a] (let [d (- a mx)] (* d d))) xs))
                       syy (reduce + 0.0 (map (fn [b] (let [d (- b my)] (* d d))) ys))]
                   (when (zero? sxx) (f/domain-error! :div0))
                   (val/number (Math/sqrt
                                (/ (- syy (/ (* sxy sxy) sxx))
                                   (double (- n 2))))))))
             :arity [2 2])

(f/register! "TREND"
  ;; Minimal TREND: with 1-arg behaviour like FORECAST at each known-x.
  ;; We return the fitted y for the first new_x (scalar) — covers the
  ;; common `TREND(known_y, known_x, new_x)` pattern.
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 1)])
                     ys (f/collect-finite-numerics [(nth args 0)])
                     new-x (if (> (count args) 2) (f/num! (nth args 2))
                               (double (last xs)))]
                 (when (not= (count xs) (count ys)) (f/domain-error! :na))
                 (let [[m b] (slope-intercept xs ys)]
                   (val/number (+ b (* m new-x))))))
             :arity [1 4])

(f/register! "GROWTH"
  ;; Exponential fit: fits y = b*m^x via log-linear regression, then
  ;; evaluates at new_x.
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 1)])
                     ys (f/collect-finite-numerics [(nth args 0)])
                     new-x (if (> (count args) 2) (f/num! (nth args 2))
                               (double (last xs)))]
                 (when (not= (count xs) (count ys)) (f/domain-error! :na))
                 (when (some #(<= (double %) 0.0) ys) (f/domain-error! :num))
                 (let [lys (mapv #(Math/log (double %)) ys)
                       [m b] (slope-intercept xs lys)]
                   (val/number (Math/exp (+ b (* m new-x)))))))
             :arity [1 4])

;; ---------------------------------------------------------------------------
;; STANDARDIZE / FISHER / FISHERINV

(f/register! "STANDARDIZE"
             ^{:scalar? true}
             (fn [args]
               (let [x  (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))]
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (/ (- x mu) sd))))
             :arity [3 3])

(f/register! "FISHER"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (when-not (< -1.0 x 1.0) (f/domain-error! :num))
                 (val/number (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x)))))))
             :arity [1 1])

(f/register! "FISHERINV"
             ^{:scalar? true}
             (fn [args]
               (let [y (f/num! (nth args 0))
                     e (Math/exp (* 2.0 y))]
                 (val/number (/ (- e 1.0) (+ e 1.0)))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Conditional aggregates — COUNTIF / SUMIF / AVERAGEIF and *IFS family.
;;
;; POI's match semantics:
;;   - Number criterion: exact match (double equality)
;;   - Boolean criterion: exact match
;;   - String criterion: may start with op (<, >, <=, >=, <>, =); the
;;     remainder is compared either numerically (if parsable on both
;;     sides) or textually (case-insensitive). Wildcards (`*`, `?`) work
;;     in = / <> / bare string matches.

(defn- wildcard->regex [s]
  (let [sb (p/sb)]
    (loop [i 0]
      (when (< i (count s))
        (let [c (.charAt ^String s i)]
          (cond
            (= c \*) (p/sb-append! sb ".*")
            (= c \?) (p/sb-append! sb ".")
            (= c \~) (when (< (inc i) (count s))
                       (let [nxt (.charAt ^String s (inc i))]
                         (p/sb-append! sb (p/regex-quote (str nxt)))))
            :else    (p/sb-append! sb (p/regex-quote (str c))))
          (recur (if (= c \~) (+ i 2) (inc i))))))
    (p/pattern-icase (p/sb->str sb))))

(defn- parse-num-maybe [s] (p/parse-double s))

(defn- parse-criterion
  "Decode a criterion cell into `[op rhs]` where op is :eq :ne :lt :le
  :gt :ge and rhs is either a double or a string. Numbers, booleans and
  blank values decode to :eq of themselves."
  [v]
  (cond
    (val/num? v)   [:eq (double (:v v))]
    (val/bool? v)  [:eq (boolean (:v v))]
    (val/blank? v) [:eq ""]
    (val/str? v)
    (let [s (:v v)
          [op rest]
          (cond
            (str/starts-with? s ">=") [:ge (subs s 2)]
            (str/starts-with? s "<=") [:le (subs s 2)]
            (str/starts-with? s "<>") [:ne (subs s 2)]
            (str/starts-with? s ">")  [:gt (subs s 1)]
            (str/starts-with? s "<")  [:lt (subs s 1)]
            (str/starts-with? s "=")  [:eq (subs s 1)]
            :else                     [:eq s])]
      [op (or (parse-num-maybe rest) rest)])
    :else [:eq v]))

(defn- cell-val
  "Extract a scalar or nil from a cell inside an area/ref."
  [v]
  (case (:t v)
    :blank nil
    :num   (double (:v v))
    :bool  (boolean (:v v))
    :str   (:v v)
    :err   (:v v)
    nil))

(defn- criterion-matches?
  [cell [op rhs]]
  (cond
    (and (= op :eq) (string? rhs))
    (cond
      (nil? cell)    (= "" rhs)
      :else          (let [re (wildcard->regex rhs)
                           s  (cond
                                (boolean? cell) (if cell "TRUE" "FALSE")
                                (number? cell)  (if (== (double (long cell)) (double cell))
                                                  (str (long cell)) (str cell))
                                :else (str cell))]
                       (some? (re-matches re s))))

    (and (= op :ne) (string? rhs))
    (not (criterion-matches? cell [:eq rhs]))

    ;; rhs is number → need to coerce cell to number
    (number? rhs)
    (let [cn (cond (number? cell) (double cell)
                   (string? cell) (parse-num-maybe cell)
                   (boolean? cell) (if cell 1.0 0.0)
                   :else nil)]
      (when (some? cn)
        (case op
          :eq (== (double cn) (double rhs))
          :ne (not (== (double cn) (double rhs)))
          :lt (< (double cn) (double rhs))
          :le (<= (double cn) (double rhs))
          :gt (> (double cn) (double rhs))
          :ge (>= (double cn) (double rhs)))))

    ;; rhs is string with ordering op → case-insensitive compare
    (string? rhs)
    (let [cs (cond (nil? cell) ""
                   (string? cell) cell
                   :else (str cell))
          a (str/lower-case cs)
          b (str/lower-case rhs)
          c (compare a b)]
      (case op
        :lt (neg? c)
        :le (not (pos? c))
        :gt (pos? c)
        :ge (not (neg? c))
        nil))

    (boolean? rhs)
    (case op
      :eq (= cell rhs)
      :ne (not= cell rhs)
      nil)))

(defn- area-cells
  "Return a vector of cell-values (areas expanded row-major, refs
  resolved, blanks preserved as BLANK)."
  [v]
  (let [t (volatile! (transient []))]
    (f/each-scalar [v] (fn [c _] (vswap! t conj! c)))
    (persistent! @t)))

(defn- count-if-over [cells criterion]
  (let [crit (parse-criterion criterion)
        n (volatile! 0)]
    (doseq [c cells]
      (when (criterion-matches? (cell-val c) crit)
        (vswap! n inc)))
    @n))

(f/register! "COUNTIF"
             (fn [args]
               (val/number (double (count-if-over (area-cells (nth args 0))
                                                  (nth args 1)))))
             :arity [2 2])

(defn- ifs-indices
  "Given [range1 crit1 range2 crit2 ...] return the indices where every
  (cell, criterion) pair matches. The ranges must all have the same
  number of cells; else #VALUE!."
  [pairs]
  (let [cellses (mapv (fn [[r _]] (area-cells r)) pairs)
        crits   (mapv (fn [[_ c]] (parse-criterion c)) pairs)
        n (count (first cellses))]
    (when (some #(not= n (count %)) cellses)
      (f/domain-error! :value))
    (filter
     (fn [i]
       (every? (fn [[cells crit]]
                 (criterion-matches? (cell-val (nth cells i)) crit))
               (map vector cellses crits)))
     (range n))))

(f/register! "COUNTIFS"
             (fn [args]
               (when (odd? (count args)) (f/domain-error! :value))
               (val/number (double (count (ifs-indices (partition 2 args))))))
             :arity [2 nil])

(defn- numeric-at [cells i]
  (let [c (cell-val (nth cells i))]
    (cond (number? c) (double c)
          (string? c) (parse-num-maybe c)
          (boolean? c) (if c 1.0 0.0)
          :else nil)))

(f/register! "SUMIF"
  ;; SUMIF(range, criterion, [sum_range])
             (fn [args]
               (let [range-cells (area-cells (nth args 0))
                     crit        (parse-criterion (nth args 1))
                     sum-cells   (if (> (count args) 2)
                                   (area-cells (nth args 2))
                                   range-cells)
                     total (volatile! 0.0)]
                 (dotimes [i (count range-cells)]
                   (when (and (< i (count sum-cells))
                              (criterion-matches? (cell-val (nth range-cells i)) crit))
                     (when-let [x (numeric-at sum-cells i)]
                       (vswap! total + x))))
                 (val/number @total)))
             :arity [2 3])

(f/register! "SUMIFS"
  ;; SUMIFS(sum_range, crit_range1, crit1, crit_range2, crit2, ...)
             (fn [args]
               (let [sum-cells (area-cells (nth args 0))
                     pairs     (partition 2 (drop 1 args))
                     _ (when (not (even? (count (drop 1 args))))
                         (f/domain-error! :value))
                     indices   (ifs-indices pairs)
                     total (volatile! 0.0)]
                 (doseq [i indices]
                   (when (< i (count sum-cells))
                     (when-let [x (numeric-at sum-cells i)]
                       (vswap! total + x))))
                 (val/number @total)))
             :arity [3 nil])

(f/register! "AVERAGEIF"
             (fn [args]
               (let [range-cells (area-cells (nth args 0))
                     crit        (parse-criterion (nth args 1))
                     avg-cells   (if (> (count args) 2)
                                   (area-cells (nth args 2))
                                   range-cells)
                     total (volatile! 0.0)
                     n (volatile! 0)]
                 (dotimes [i (count range-cells)]
                   (when (and (< i (count avg-cells))
                              (criterion-matches? (cell-val (nth range-cells i)) crit))
                     (when-let [x (numeric-at avg-cells i)]
                       (vswap! total + x)
                       (vswap! n inc))))
                 (if (zero? @n) val/ERR-DIV0
                     (val/number (/ @total (double @n))))))
             :arity [2 3])

(f/register! "AVERAGEIFS"
             (fn [args]
               (let [avg-cells (area-cells (nth args 0))
                     pairs     (partition 2 (drop 1 args))
                     _ (when (not (even? (count (drop 1 args))))
                         (f/domain-error! :value))
                     indices   (ifs-indices pairs)
                     total (volatile! 0.0)
                     n (volatile! 0)]
                 (doseq [i indices]
                   (when (< i (count avg-cells))
                     (when-let [x (numeric-at avg-cells i)]
                       (vswap! total + x)
                       (vswap! n inc))))
                 (if (zero? @n) val/ERR-DIV0
                     (val/number (/ @total (double @n))))))
             :arity [3 nil])

(f/register! "MAXIFS"
             (fn [args]
               (let [max-cells (area-cells (nth args 0))
                     pairs     (partition 2 (drop 1 args))
                     indices   (ifs-indices pairs)
                     vals (keep (fn [i] (when (< i (count max-cells))
                                          (numeric-at max-cells i)))
                                indices)]
                 (if (empty? vals) (val/number 0.0)
                     (val/number (apply max vals)))))
             :arity [3 nil])

(f/register! "MINIFS"
             (fn [args]
               (let [min-cells (area-cells (nth args 0))
                     pairs     (partition 2 (drop 1 args))
                     indices   (ifs-indices pairs)
                     vals (keep (fn [i] (when (< i (count min-cells))
                                          (numeric-at min-cells i)))
                                indices)]
                 (if (empty? vals) (val/number 0.0)
                     (val/number (apply min vals)))))
             :arity [3 nil])

;; ---------------------------------------------------------------------------
;; Probability / distribution — normal
;;
;; All derived from two primitives: a rational-approximation `phi-cdf`
;; and its inverse `probit` (Beasley-Springer-Moro). Numerical accuracy
;; is ~1e-9 for CDF and ~1e-7 for the inverse — good enough for Excel.

(defn- norm-pdf ^double [^double x ^double mu ^double sd]
  (/ (Math/exp (* -0.5 (let [z (/ (- x mu) sd)] (* z z))))
     (* sd (Math/sqrt (* 2.0 Math/PI)))))

(defn- phi-std
  "Standard normal CDF via the Abramowitz & Stegun 26.2.17 formula
  combined with Boole's reflection for negative z. Accuracy ≈ 7.5e-8."
  ^double [^double z]
  (if (neg? z)
    (- 1.0 (phi-std (- z)))
    (let [b0 0.2316419 b1 0.319381530 b2 -0.356563782
          b3 1.781477937 b4 -1.821255978 b5 1.330274429
          t  (/ 1.0 (+ 1.0 (* b0 z)))
          pdf (/ (Math/exp (* -0.5 z z)) (Math/sqrt (* 2.0 Math/PI)))]
      (- 1.0 (* pdf
                (+ (* b1 t)
                   (* b2 t t)
                   (* b3 t t t)
                   (* b4 t t t t)
                   (* b5 t t t t t)))))))

(defn- norm-cdf ^double [^double x ^double mu ^double sd]
  (phi-std (/ (- x mu) sd)))

(defn- probit
  "Standard-normal inverse CDF via Beasley-Springer-Moro.
  Relative error ≈ 1.15e-9 for p in (0, 1)."
  ^double [^double p]
  (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
  (let [a [-3.969683028665376e+01 2.209460984245205e+02
           -2.759285104469687e+02 1.383577518672690e+02
           -3.066479806614716e+01 2.506628277459239e+00]
        b [-5.447609879822406e+01 1.615858368580409e+02
           -1.556989798598866e+02 6.680131188771972e+01
           -1.328068155288572e+01]
        c [-7.784894002430293e-03 -3.223964580411365e-01
           -2.400758277161838e+00 -2.549732539343734e+00
           4.374664141464968e+00 2.938163982698783e+00]
        d [7.784695709041462e-03 3.224671290700398e-01
           2.445134137142996e+00 3.754408661907416e+00]
        plow 0.02425 phigh (- 1.0 plow)]
    (cond
      (< p plow)
      (let [q (Math/sqrt (* -2.0 (Math/log p)))]
        (/ (+ (* (+ (* (+ (* (+ (* (+ (* (nth c 0) q) (nth c 1)) q)
                                (nth c 2)) q) (nth c 3)) q) (nth c 4)) q) (nth c 5))
           (+ (* (+ (* (+ (* (+ (* (nth d 0) q) (nth d 1)) q)
                          (nth d 2)) q) (nth d 3)) q) 1.0)))

      (<= p phigh)
      (let [q (- p 0.5) r (* q q)]
        (/ (* (+ (* (+ (* (+ (* (+ (* (+ (* (nth a 0) r) (nth a 1)) r)
                                   (nth a 2)) r) (nth a 3)) r) (nth a 4)) r) (nth a 5))
              q)
           (+ (* (+ (* (+ (* (+ (* (+ (* (nth b 0) r) (nth b 1)) r)
                                (nth b 2)) r) (nth b 3)) r) (nth b 4)) r) 1.0)))

      :else
      (let [q (Math/sqrt (* -2.0 (Math/log (- 1.0 p))))]
        (- (/ (+ (* (+ (* (+ (* (+ (* (+ (* (nth c 0) q) (nth c 1)) q)
                                   (nth c 2)) q) (nth c 3)) q) (nth c 4)) q) (nth c 5))
              (+ (* (+ (* (+ (* (+ (* (nth d 0) q) (nth d 1)) q)
                             (nth d 2)) q) (nth d 3)) q) 1.0)))))))

(defn- norm-dist-impl [x mu sd cum?]
  (when (<= sd 0.0) (f/domain-error! :num))
  (if cum?
    (norm-cdf x mu sd)
    (norm-pdf x mu sd)))

(f/register! "NORMDIST"
             ^{:scalar? true}
             (fn [args]
               (val/number (norm-dist-impl (f/num! (nth args 0))
                                           (f/num! (nth args 1))
                                           (f/num! (nth args 2))
                                           (f/bool! (nth args 3)))))
             :arity [4 4])

(f/register! "NORM.DIST"
             ^{:scalar? true}
             (fn [args]
               (val/number (norm-dist-impl (f/num! (nth args 0))
                                           (f/num! (nth args 1))
                                           (f/num! (nth args 2))
                                           (f/bool! (nth args 3)))))
             :arity [4 4])

(f/register! "NORMSDIST"
             ^{:scalar? true}
             (fn [args] (val/number (phi-std (f/num! (nth args 0)))))
             :arity [1 1])

(f/register! "NORM.S.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [z (f/num! (nth args 0))
                     cum? (if (> (count args) 1) (f/bool! (nth args 1)) true)]
                 (val/number (if cum?
                               (phi-std z)
                               (norm-pdf z 0.0 1.0)))))
             :arity [1 2])

(f/register! "NORMINV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))]
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (+ mu (* sd (probit p))))))
             :arity [3 3])

(f/register! "NORM.INV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))]
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (+ mu (* sd (probit p))))))
             :arity [3 3])

(f/register! "NORMSINV"
             ^{:scalar? true}
             (fn [args] (val/number (probit (f/num! (nth args 0)))))
             :arity [1 1])

(f/register! "NORM.S.INV"
             ^{:scalar? true}
             (fn [args] (val/number (probit (f/num! (nth args 0)))))
             :arity [1 1])

(f/register! "CONFIDENCE"
             ^{:scalar? true}
             (fn [args]
               (let [a (f/num! (nth args 0))
                     sd (f/num! (nth args 1))
                     n  (f/num! (nth args 2))]
                 (when (or (<= a 0.0) (>= a 1.0)) (f/domain-error! :num))
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (when (< n 1.0) (f/domain-error! :num))
                 (val/number (* (probit (- 1.0 (/ a 2.0)))
                                (/ sd (Math/sqrt n))))))
             :arity [3 3])

(f/register! "CONFIDENCE.NORM"
             ^{:scalar? true}
             (fn [args]
               (let [a (f/num! (nth args 0))
                     sd (f/num! (nth args 1))
                     n  (f/num! (nth args 2))]
                 (when (or (<= a 0.0) (>= a 1.0)) (f/domain-error! :num))
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (when (< n 1.0) (f/domain-error! :num))
                 (val/number (* (probit (- 1.0 (/ a 2.0)))
                                (/ sd (Math/sqrt n))))))
             :arity [3 3])

(f/register! "ZTEST"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     x  (f/num! (nth args 1))
                     n  (count xs)
                     sd (if (> (count args) 2)
                          (f/num! (nth args 2))
                          (Math/sqrt (variance xs true)))]
                 (when (zero? n) (f/domain-error! :na))
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (let [mu (/ (reduce + 0.0 xs) (double n))
                       z  (/ (- mu x) (/ sd (Math/sqrt n)))]
                   (val/number (- 1.0 (phi-std z))))))
             :arity [2 3])

(f/register! "Z.TEST"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     x  (f/num! (nth args 1))
                     n  (count xs)
                     sd (if (> (count args) 2)
                          (f/num! (nth args 2))
                          (Math/sqrt (variance xs true)))]
                 (when (zero? n) (f/domain-error! :na))
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (let [mu (/ (reduce + 0.0 xs) (double n))
                       z  (/ (- mu x) (/ sd (Math/sqrt n)))]
                   (val/number (- 1.0 (phi-std z))))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; Lognormal

(f/register! "LOGNORMDIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))]
                 (when (<= x 0.0) (f/domain-error! :num))
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (phi-std (/ (- (Math/log x) mu) sd)))))
             :arity [3 3])

(f/register! "LOGNORM.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (<= x 0.0) (f/domain-error! :num))
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (if cum?
                               (phi-std (/ (- (Math/log x) mu) sd))
                               (/ (norm-pdf (Math/log x) mu sd) x)))))
             :arity [4 4])

(f/register! "LOGINV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))]
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (Math/exp (+ mu (* sd (probit p)))))))
             :arity [3 3])

(f/register! "LOGNORM.INV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     mu (f/num! (nth args 1))
                     sd (f/num! (nth args 2))]
                 (when (<= sd 0.0) (f/domain-error! :num))
                 (val/number (Math/exp (+ mu (* sd (probit p)))))))
             :arity [3 3])

;; ---------------------------------------------------------------------------
;; Exponential & Weibull

(f/register! "EXPONDIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     lambda (f/num! (nth args 1))
                     cum? (f/bool! (nth args 2))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (<= lambda 0.0) (f/domain-error! :num))
                 (val/number (if cum?
                               (- 1.0 (Math/exp (- (* lambda x))))
                               (* lambda (Math/exp (- (* lambda x))))))))
             :arity [3 3])

(f/register! "EXPON.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     lambda (f/num! (nth args 1))
                     cum? (f/bool! (nth args 2))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (<= lambda 0.0) (f/domain-error! :num))
                 (val/number (if cum?
                               (- 1.0 (Math/exp (- (* lambda x))))
                               (* lambda (Math/exp (- (* lambda x))))))))
             :arity [3 3])

(f/register! "WEIBULL"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number (if cum?
                               (- 1.0 (Math/exp (- (Math/pow (/ x b) a))))
                               (* (/ a (Math/pow b a))
                                  (Math/pow x (dec a))
                                  (Math/exp (- (Math/pow (/ x b) a))))))))
             :arity [4 4])

(f/register! "WEIBULL.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number (if cum?
                               (- 1.0 (Math/exp (- (Math/pow (/ x b) a))))
                               (* (/ a (Math/pow b a))
                                  (Math/pow x (dec a))
                                  (Math/exp (- (Math/pow (/ x b) a))))))))
             :arity [4 4])

;; ---------------------------------------------------------------------------
;; Poisson

(defn- log-fact ^double [^long n]
  (if (<= n 1) 0.0
      (loop [k 2 acc 0.0]
        (if (> k n) acc (recur (inc k) (+ acc (Math/log k)))))))

(defn- poisson-pmf ^double [^double lambda ^long k]
  (Math/exp (- (- (* k (Math/log lambda))
                  (log-fact k))
               lambda)))

(f/register! "POISSON"
             ^{:scalar? true}
             (fn [args]
               (let [k (long (f/num! (nth args 0)))
                     lambda (f/num! (nth args 1))
                     cum? (f/bool! (nth args 2))]
                 (when (< k 0) (f/domain-error! :num))
                 (when (<= lambda 0.0) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (reduce + 0.0 (map #(poisson-pmf lambda %) (range 0 (inc k))))
                    (poisson-pmf lambda k)))))
             :arity [3 3])

(f/register! "POISSON.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [k (long (f/num! (nth args 0)))
                     lambda (f/num! (nth args 1))
                     cum? (f/bool! (nth args 2))]
                 (when (< k 0) (f/domain-error! :num))
                 (when (<= lambda 0.0) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (reduce + 0.0 (map #(poisson-pmf lambda %) (range 0 (inc k))))
                    (poisson-pmf lambda k)))))
             :arity [3 3])

;; ---------------------------------------------------------------------------
;; Binomial / negative-binomial / hypergeometric

(defn- log-choose ^double [^long n ^long k]
  (cond
    (or (neg? k) (< n k)) ##-Inf
    (zero? k) 0.0
    :else (- (log-fact n) (log-fact k) (log-fact (- n k)))))

(defn- binom-pmf ^double [^long n ^long k ^double p]
  (cond
    (or (neg? k) (> k n)) 0.0
    (or (= p 0.0) (= p 1.0))
    (cond (and (= p 0.0) (= k 0)) 1.0
          (and (= p 1.0) (= k n)) 1.0
          :else 0.0)
    :else (Math/exp (+ (log-choose n k)
                       (* k (Math/log p))
                       (* (- n k) (Math/log (- 1.0 p)))))))

(f/register! "BINOMDIST"
             ^{:scalar? true}
             (fn [args]
               (let [k (long (f/num! (nth args 0)))
                     n (long (f/num! (nth args 1)))
                     p (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (or (< k 0) (> k n)) (f/domain-error! :num))
                 (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (reduce + 0.0 (map #(binom-pmf n % p) (range 0 (inc k))))
                    (binom-pmf n k p)))))
             :arity [4 4])

(f/register! "BINOM.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [k (long (f/num! (nth args 0)))
                     n (long (f/num! (nth args 1)))
                     p (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (or (< k 0) (> k n)) (f/domain-error! :num))
                 (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (reduce + 0.0 (map #(binom-pmf n % p) (range 0 (inc k))))
                    (binom-pmf n k p)))))
             :arity [4 4])

(f/register! "CRITBINOM"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))
                     p (f/num! (nth args 1))
                     alpha (f/num! (nth args 2))]
                 (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (when (or (< alpha 0.0) (> alpha 1.0)) (f/domain-error! :num))
                 (loop [k 0 acc 0.0]
                   (let [acc' (+ acc (binom-pmf n k p))]
                     (if (or (>= acc' alpha) (= k n))
                       (val/number (double k))
                       (recur (inc k) acc'))))))
             :arity [3 3])

(f/register! "BINOM.INV"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))
                     p (f/num! (nth args 1))
                     alpha (f/num! (nth args 2))]
                 (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (when (or (< alpha 0.0) (> alpha 1.0)) (f/domain-error! :num))
                 (loop [k 0 acc 0.0]
                   (let [acc' (+ acc (binom-pmf n k p))]
                     (if (or (>= acc' alpha) (= k n))
                       (val/number (double k))
                       (recur (inc k) acc'))))))
             :arity [3 3])

(f/register! "NEGBINOMDIST"
             ^{:scalar? true}
             (fn [args]
               (let [f (long (f/num! (nth args 0)))
                     s (long (f/num! (nth args 1)))
                     p (f/num! (nth args 2))]
                 (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (val/number
                  (Math/exp (+ (log-choose (+ f s -1) (dec s))
                               (* s (Math/log p))
                               (* f (Math/log (- 1.0 p))))))))
             :arity [3 3])

(f/register! "NEGBINOM.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [f (long (f/num! (nth args 0)))
                     s (long (f/num! (nth args 1)))
                     p (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (or (< p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (reduce + 0.0
                            (for [k (range 0 (inc f))]
                              (Math/exp (+ (log-choose (+ k s -1) (dec s))
                                           (* s (Math/log p))
                                           (* k (Math/log (- 1.0 p)))))))
                    (Math/exp (+ (log-choose (+ f s -1) (dec s))
                                 (* s (Math/log p))
                                 (* f (Math/log (- 1.0 p)))))))))
             :arity [4 4])

(f/register! "HYPGEOMDIST"
             ^{:scalar? true}
             (fn [args]
               (let [k (long (f/num! (nth args 0)))
                     n (long (f/num! (nth args 1)))
                     K (long (f/num! (nth args 2)))
                     N (long (f/num! (nth args 3)))]
                 (val/number
                  (Math/exp (- (+ (log-choose K k) (log-choose (- N K) (- n k)))
                               (log-choose N n))))))
             :arity [4 4])

(f/register! "HYPGEOM.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [k (long (f/num! (nth args 0)))
                     n (long (f/num! (nth args 1)))
                     K (long (f/num! (nth args 2)))
                     N (long (f/num! (nth args 3)))
                     cum? (f/bool! (nth args 4))]
                 (val/number
                  (if cum?
                    (reduce + 0.0
                            (for [i (range 0 (inc k))]
                              (Math/exp (- (+ (log-choose K i)
                                              (log-choose (- N K) (- n i)))
                                           (log-choose N n)))))
                    (Math/exp (- (+ (log-choose K k)
                                    (log-choose (- N K) (- n k)))
                                 (log-choose N n)))))))
             :arity [5 5])

;; ---------------------------------------------------------------------------
;; Gamma / Beta / Chi-squared / F / T — use log-gamma (Lanczos)

(def ^:private lanczos-g 7.0)
(def ^:private lanczos-coeffs
  [0.99999999999980993
   676.5203681218851
   -1259.1392167224028
   771.32342877765313
   -176.61502916214059
   12.507343278686905
   -0.13857109526572012
   9.9843695780195716e-6
   1.5056327351493116e-7])

(defn- log-gamma ^double [^double z]
  (if (< z 0.5)
    (- (Math/log (/ Math/PI (Math/sin (* Math/PI z))))
       (log-gamma (- 1.0 z)))
    (let [z (- z 1.0)
          base (+ z lanczos-g 0.5)
          sum (reduce + (first lanczos-coeffs)
                      (map-indexed (fn [i c] (/ c (+ z (double (inc i)))))
                                   (rest lanczos-coeffs)))]
      (+ (* 0.5 (Math/log (* 2.0 Math/PI)))
         (* (+ z 0.5) (Math/log base))
         (- base)
         (Math/log sum)))))

(defn- gamma ^double [^double z] (Math/exp (log-gamma z)))

(f/register! "GAMMALN"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (when (<= x 0.0) (f/domain-error! :num))
                 (val/number (log-gamma x))))
             :arity [1 1])

(f/register! "GAMMALN.PRECISE"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (when (<= x 0.0) (f/domain-error! :num))
                 (val/number (log-gamma x))))
             :arity [1 1])

(f/register! "GAMMA"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (when (or (<= x 0.0)
                           (and (== x (double (long x)))
                                (<= x 0.0)))
                   (f/domain-error! :num))
                 (val/number (gamma x))))
             :arity [1 1])

;; Regularised lower incomplete gamma P(a, x) via series / continued
;; fraction (Numerical Recipes).

(defn- gser ^double [^double a ^double x]
  (let [max-iter 200 eps 3.0e-14]
    (if (<= x 0.0) 0.0
        (loop [n 0 ap a sum (/ 1.0 a) term (/ 1.0 a)]
          (let [ap' (inc ap)
                term' (* term (/ x ap'))
                sum' (+ sum term')]
            (cond
              (< (Math/abs term') (* (Math/abs sum') eps))
              (* sum' (Math/exp (- (* a (Math/log x)) x (log-gamma a))))
              (>= n max-iter) (* sum' (Math/exp (- (* a (Math/log x)) x (log-gamma a))))
              :else (recur (inc n) ap' sum' term')))))))

(defn- gcf ^double [^double a ^double x]
  (let [max-iter 200 eps 3.0e-14 fpmin 1.0e-300]
    (loop [i 1
           b (+ (- x a) 1.0)
           c (/ 1.0 fpmin)
           d (/ 1.0 b)
           h (/ 1.0 b)]
      (if (> i max-iter)
        (* (Math/exp (- (* a (Math/log x)) x (log-gamma a))) h)
        (let [an (* (- i) (- i a))
              b' (+ b 2.0)
              d1 (let [v (+ (* an d) b')]
                   (if (< (Math/abs v) fpmin) fpmin v))
              c1 (let [v (+ b' (/ an c))]
                   (if (< (Math/abs v) fpmin) fpmin v))
              d2 (/ 1.0 d1)
              del (* d2 c1)
              h' (* h del)]
          (if (< (Math/abs (- del 1.0)) eps)
            (* (Math/exp (- (* a (Math/log x)) x (log-gamma a))) h')
            (recur (inc i) b' c1 d2 h')))))))

(defn- gamma-p
  "Regularised lower incomplete gamma P(a, x)."
  ^double [^double a ^double x]
  (cond
    (or (< x 0.0) (<= a 0.0)) 0.0
    (< x (inc a)) (gser a x)
    :else (- 1.0 (gcf a x))))

(defn- gamma-q ^double [^double a ^double x] (- 1.0 (gamma-p a x)))

(f/register! "GAMMADIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (or (< x 0.0) (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (gamma-p a (/ x b))
                    (/ (* (Math/pow (/ x b) (dec a))
                          (Math/exp (- (/ x b))))
                       (* b (gamma a)))))))
             :arity [4 4])

(f/register! "GAMMA.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (or (< x 0.0) (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (gamma-p a (/ x b))
                    (/ (* (Math/pow (/ x b) (dec a))
                          (Math/exp (- (/ x b))))
                       (* b (gamma a)))))))
             :arity [4 4])

;; Inverse gamma CDF via bisection — not the fastest, accurate to ~1e-9.
(defn- invert-monotone
  "Bisection inverse of a monotone-increasing `f` on (lo, hi) such that
  f(lo) < target < f(hi). Tolerance in target. Exits when range shrinks
  below 1e-12 as well."
  ^double [f ^double lo ^double hi ^double target]
  (loop [a lo b hi i 0]
    (let [m (/ (+ a b) 2.0)
          fm (f m)]
      (cond
        (or (< (Math/abs (- fm target)) 1.0e-10) (< (- b a) 1.0e-12) (> i 100)) m
        (< fm target) (recur m b (inc i))
        :else         (recur a m (inc i))))))

(f/register! "GAMMAINV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number
                  (* b (invert-monotone #(gamma-p a %) 0.0 (+ 20.0 (* 20.0 a)) p)))))
             :arity [3 3])

(f/register! "GAMMA.INV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number
                  (* b (invert-monotone #(gamma-p a %) 0.0 (+ 20.0 (* 20.0 a)) p)))))
             :arity [3 3])

(f/register! "CHIDIST"
  ;; Right-tailed chi-squared with `df` degrees of freedom.
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (gamma-q (/ df 2.0) (/ x 2.0)))))
             :arity [2 2])

(f/register! "CHISQ.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     df (f/num! (nth args 1))
                     cum? (f/bool! (nth args 2))]
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (gamma-p (/ df 2.0) (/ x 2.0))
                    (/ (* (Math/pow x (- (/ df 2.0) 1.0))
                          (Math/exp (/ (- x) 2.0)))
                       (* (Math/pow 2.0 (/ df 2.0)) (gamma (/ df 2.0))))))))
             :arity [3 3])

(f/register! "CHISQ.DIST.RT"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (gamma-q (/ df 2.0) (/ x 2.0)))))
             :arity [2 2])

(f/register! "CHIINV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (or (<= p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number
                  (* 2.0 (invert-monotone #(gamma-p (/ df 2.0) %)
                                          0.0 (+ 50.0 (* 10.0 df))
                                          (- 1.0 p))))))
             :arity [2 2])

(f/register! "CHISQ.INV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number
                  (* 2.0 (invert-monotone #(gamma-p (/ df 2.0) %)
                                          0.0 (+ 50.0 (* 10.0 df)) p)))))
             :arity [2 2])

(f/register! "CHISQ.INV.RT"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number
                  (* 2.0 (invert-monotone #(gamma-p (/ df 2.0) %)
                                          0.0 (+ 50.0 (* 10.0 df))
                                          (- 1.0 p))))))
             :arity [2 2])

(f/register! "CHITEST"
  ;; Chi-squared test of independence across two equal-shape ranges.
  ;; Returns the right-tail p-value for the statistic.
             (fn [args]
               (let [obs (f/collect-finite-numerics [(nth args 0)])
                     exp (f/collect-finite-numerics [(nth args 1)])]
                 (when (not= (count obs) (count exp)) (f/domain-error! :na))
                 (when (some #(<= (double %) 0.0) exp) (f/domain-error! :num))
                 (let [chi (reduce + 0.0
                                   (map (fn [o e] (/ (let [d (- o e)] (* d d)) e))
                                        obs exp))
            ;; When shape is unavailable we fall back to (n-1); POI uses
            ;; (rows-1)*(cols-1) from the area shape. Approximation.
                       df (dec (count obs))]
                   (val/number (gamma-q (/ df 2.0) (/ chi 2.0))))))
             :arity [2 2])

(f/register! "CHISQ.TEST"
             (fn [args]
               (let [obs (f/collect-finite-numerics [(nth args 0)])
                     exp (f/collect-finite-numerics [(nth args 1)])]
                 (when (not= (count obs) (count exp)) (f/domain-error! :na))
                 (when (some #(<= (double %) 0.0) exp) (f/domain-error! :num))
                 (let [chi (reduce + 0.0
                                   (map (fn [o e] (/ (let [d (- o e)] (* d d)) e))
                                        obs exp))
                       df (dec (count obs))]
                   (val/number (gamma-q (/ df 2.0) (/ chi 2.0))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; Regularised incomplete beta — backbone for beta, F and t distributions.

(defn- betacf ^double [^double a ^double b ^double x]
  (let [max-iter 200 eps 3.0e-14 fpmin 1.0e-300
        qab (+ a b) qap (+ a 1.0) qam (- a 1.0)
        c 1.0
        d (let [v (- 1.0 (/ (* qab x) qap))]
            (if (< (Math/abs v) fpmin) fpmin v))
        d (/ 1.0 d)]
    (loop [m 1 c c d d h d]
      (if (> m max-iter) h
          (let [m2 (* 2 m)
                aa1 (/ (* m (- b m) x) (* (+ qam m2) (+ a m2)))
                d1 (let [v (+ 1.0 (* aa1 d))]
                     (if (< (Math/abs v) fpmin) fpmin v))
                c1 (let [v (+ 1.0 (/ aa1 c))]
                     (if (< (Math/abs v) fpmin) fpmin v))
                d1 (/ 1.0 d1)
                h1 (* h d1 c1)
                aa2 (- (/ (* (+ a m) (+ qab m) x)
                          (* (+ a m2) (+ qap m2))))
                d2 (let [v (+ 1.0 (* aa2 d1))]
                     (if (< (Math/abs v) fpmin) fpmin v))
                c2 (let [v (+ 1.0 (/ aa2 c1))]
                     (if (< (Math/abs v) fpmin) fpmin v))
                d2 (/ 1.0 d2)
                h2 (* h1 d2 c2)
                del (* d2 c2)]
            (if (< (Math/abs (- del 1.0)) eps) h2
                (recur (inc m) c2 d2 h2)))))))

(defn- betai
  "Regularised incomplete beta I_x(a, b)."
  ^double [^double a ^double b ^double x]
  (cond
    (or (< x 0.0) (> x 1.0)) (f/domain-error! :num)
    (or (= x 0.0) (= x 1.0)) x
    :else
    (let [bt (Math/exp (+ (log-gamma (+ a b))
                          (- (log-gamma a))
                          (- (log-gamma b))
                          (* a (Math/log x))
                          (* b (Math/log (- 1.0 x)))))]
      (if (< x (/ (+ a 1.0) (+ a b 2.0)))
        (/ (* bt (betacf a b x)) a)
        (- 1.0 (/ (* bt (betacf b a (- 1.0 x))) b))))))

;; ---------------------------------------------------------------------------
;; t-distribution

(defn- t-cdf ^double [^double t ^double df]
  (let [x (/ df (+ df (* t t)))]
    (if (>= t 0.0)
      (- 1.0 (* 0.5 (betai (/ df 2.0) 0.5 x)))
      (* 0.5 (betai (/ df 2.0) 0.5 x)))))

(defn- t-pdf ^double [^double t ^double df]
  (* (/ (gamma (/ (inc df) 2.0))
        (* (Math/sqrt (* df Math/PI)) (gamma (/ df 2.0))))
     (Math/pow (+ 1.0 (/ (* t t) df))
               (- (/ (inc df) 2.0)))))

(f/register! "TDIST"
  ;; TDIST(x, df, tails) — x must be ≥ 0. tails is 1 or 2.
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     df (f/num! (nth args 1))
                     tails (long (f/num! (nth args 2)))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (when-not (<= 1 tails 2) (f/domain-error! :num))
                 (let [rt (- 1.0 (t-cdf x df))]
                   (val/number (if (= tails 1) rt (* 2.0 rt))))))
             :arity [3 3])

(f/register! "T.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [t (f/num! (nth args 0))
                     df (f/num! (nth args 1))
                     cum? (f/bool! (nth args 2))]
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (if cum? (t-cdf t df) (t-pdf t df)))))
             :arity [3 3])

(f/register! "T.DIST.RT"
             ^{:scalar? true}
             (fn [args]
               (let [t (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (- 1.0 (t-cdf t df)))))
             :arity [2 2])

(f/register! "T.DIST.2T"
             ^{:scalar? true}
             (fn [args]
               (let [t (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (< t 0.0) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (* 2.0 (- 1.0 (t-cdf t df))))))
             :arity [2 2])

(f/register! "TINV"
  ;; Two-tailed inverse.
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (or (<= p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
      ;; P(|T|<=t) = 1-p → P(T<=t) = 1 - p/2
                 (val/number (invert-monotone #(t-cdf % df) -50.0 50.0 (- 1.0 (/ p 2.0))))))
             :arity [2 2])

(f/register! "T.INV"
  ;; Left-tailed inverse.
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (invert-monotone #(t-cdf % df) -50.0 50.0 p))))
             :arity [2 2])

(f/register! "T.INV.2T"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     df (f/num! (nth args 1))]
                 (when (or (<= p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (when (<= df 0.0) (f/domain-error! :num))
                 (val/number (invert-monotone #(t-cdf % df) 0.0 50.0 (- 1.0 (/ p 2.0))))))
             :arity [2 2])

(f/register! "TTEST"
  ;; TTEST(array1, array2, tails, type). Types: 1 paired, 2 equal
  ;; variance, 3 unequal variance.
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     ys (f/collect-finite-numerics [(nth args 1)])
                     tails (long (f/num! (nth args 2)))
                     ttype (long (f/num! (nth args 3)))]
                 (when-not (<= 1 tails 2) (f/domain-error! :num))
                 (when-not (<= 1 ttype 3) (f/domain-error! :num))
                 (when (and (= ttype 1) (not= (count xs) (count ys)))
                   (f/domain-error! :na))
                 (let [nx (count xs) ny (count ys)
                       mx (/ (reduce + 0.0 xs) (double nx))
                       my (/ (reduce + 0.0 ys) (double ny))
                       [t df]
                       (case ttype
                         1 (let [diffs (map - xs ys)
                                 md (/ (reduce + 0.0 diffs) (double nx))
                                 sd (Math/sqrt (variance diffs true))]
                             [(/ md (/ sd (Math/sqrt nx))) (dec nx)])
                         2 (let [vx (variance xs true) vy (variance ys true)
                                 sp (/ (+ (* (dec nx) vx) (* (dec ny) vy))
                                       (double (+ nx ny -2)))
                                 se (Math/sqrt (* sp (+ (/ 1.0 nx) (/ 1.0 ny))))]
                             [(/ (- mx my) se) (+ nx ny -2)])
                         3 (let [vx (variance xs true) vy (variance ys true)
                                 vxn (/ vx (double nx)) vyn (/ vy (double ny))
                                 df (/ (Math/pow (+ vxn vyn) 2)
                                       (+ (/ (* vxn vxn) (double (dec nx)))
                                          (/ (* vyn vyn) (double (dec ny)))))]
                             [(/ (- mx my) (Math/sqrt (+ vxn vyn))) df]))
                       rt (- 1.0 (t-cdf (Math/abs (double t)) df))]
                   (val/number (if (= tails 1) rt (* 2.0 rt))))))
             :arity [4 4])

(f/register! "T.TEST"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     ys (f/collect-finite-numerics [(nth args 1)])
                     tails (long (f/num! (nth args 2)))
                     ttype (long (f/num! (nth args 3)))]
                 (when-not (<= 1 tails 2) (f/domain-error! :num))
                 (when-not (<= 1 ttype 3) (f/domain-error! :num))
                 (when (and (= ttype 1) (not= (count xs) (count ys)))
                   (f/domain-error! :na))
                 (let [nx (count xs) ny (count ys)
                       mx (/ (reduce + 0.0 xs) (double nx))
                       my (/ (reduce + 0.0 ys) (double ny))
                       [t df]
                       (case ttype
                         1 (let [diffs (map - xs ys)
                                 md (/ (reduce + 0.0 diffs) (double nx))
                                 sd (Math/sqrt (variance diffs true))]
                             [(/ md (/ sd (Math/sqrt nx))) (dec nx)])
                         2 (let [vx (variance xs true) vy (variance ys true)
                                 sp (/ (+ (* (dec nx) vx) (* (dec ny) vy))
                                       (double (+ nx ny -2)))
                                 se (Math/sqrt (* sp (+ (/ 1.0 nx) (/ 1.0 ny))))]
                             [(/ (- mx my) se) (+ nx ny -2)])
                         3 (let [vx (variance xs true) vy (variance ys true)
                                 vxn (/ vx (double nx)) vyn (/ vy (double ny))
                                 df (/ (Math/pow (+ vxn vyn) 2)
                                       (+ (/ (* vxn vxn) (double (dec nx)))
                                          (/ (* vyn vyn) (double (dec ny)))))]
                             [(/ (- mx my) (Math/sqrt (+ vxn vyn))) df]))
                       rt (- 1.0 (t-cdf (Math/abs (double t)) df))]
                   (val/number (if (= tails 1) rt (* 2.0 rt))))))
             :arity [4 4])

;; ---------------------------------------------------------------------------
;; F-distribution

(defn- f-cdf ^double [^double x ^double d1 ^double d2]
  (let [y (/ (* d1 x) (+ (* d1 x) d2))]
    (betai (/ d1 2.0) (/ d2 2.0) y)))

(f/register! "FDIST"
  ;; Right-tailed, Excel's legacy version
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     d1 (f/num! (nth args 1))
                     d2 (f/num! (nth args 2))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (or (<= d1 0.0) (<= d2 0.0)) (f/domain-error! :num))
                 (val/number (- 1.0 (f-cdf x d1 d2)))))
             :arity [3 3])

(f/register! "F.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     d1 (f/num! (nth args 1))
                     d2 (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (or (<= d1 0.0) (<= d2 0.0)) (f/domain-error! :num))
                 (val/number
                  (if cum?
                    (f-cdf x d1 d2)
                    (* (/ (gamma (/ (+ d1 d2) 2.0))
                          (* (gamma (/ d1 2.0)) (gamma (/ d2 2.0))))
                       (Math/pow (/ d1 d2) (/ d1 2.0))
                       (Math/pow x (- (/ d1 2.0) 1.0))
                       (Math/pow (+ 1.0 (* (/ d1 d2) x))
                                 (- (/ (+ d1 d2) 2.0))))))))
             :arity [4 4])

(f/register! "F.DIST.RT"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     d1 (f/num! (nth args 1))
                     d2 (f/num! (nth args 2))]
                 (when (< x 0.0) (f/domain-error! :num))
                 (when (or (<= d1 0.0) (<= d2 0.0)) (f/domain-error! :num))
                 (val/number (- 1.0 (f-cdf x d1 d2)))))
             :arity [3 3])

(f/register! "FINV"
  ;; Inverse of right-tail (legacy semantics).
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     d1 (f/num! (nth args 1))
                     d2 (f/num! (nth args 2))]
                 (when (or (<= p 0.0) (> p 1.0)) (f/domain-error! :num))
                 (when (or (<= d1 0.0) (<= d2 0.0)) (f/domain-error! :num))
                 (val/number (invert-monotone #(f-cdf % d1 d2) 0.0 1.0e6 (- 1.0 p)))))
             :arity [3 3])

(f/register! "F.INV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     d1 (f/num! (nth args 1))
                     d2 (f/num! (nth args 2))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (or (<= d1 0.0) (<= d2 0.0)) (f/domain-error! :num))
                 (val/number (invert-monotone #(f-cdf % d1 d2) 0.0 1.0e6 p))))
             :arity [3 3])

(f/register! "F.INV.RT"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     d1 (f/num! (nth args 1))
                     d2 (f/num! (nth args 2))]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (or (<= d1 0.0) (<= d2 0.0)) (f/domain-error! :num))
                 (val/number (invert-monotone #(f-cdf % d1 d2) 0.0 1.0e6 (- 1.0 p)))))
             :arity [3 3])

(f/register! "FTEST"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     ys (f/collect-finite-numerics [(nth args 1)])
                     vx (variance xs true)
                     vy (variance ys true)
                     f-stat (if (>= vx vy) (/ vx vy) (/ vy vx))
                     [d1 d2] (if (>= vx vy)
                               [(dec (count xs)) (dec (count ys))]
                               [(dec (count ys)) (dec (count xs))])]
                 (when (or (<= f-stat 0.0)) (f/domain-error! :div0))
                 (val/number (* 2.0 (- 1.0 (f-cdf f-stat d1 d2))))))
             :arity [2 2])

(f/register! "F.TEST"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     ys (f/collect-finite-numerics [(nth args 1)])
                     vx (variance xs true)
                     vy (variance ys true)
                     f-stat (if (>= vx vy) (/ vx vy) (/ vy vx))
                     [d1 d2] (if (>= vx vy)
                               [(dec (count xs)) (dec (count ys))]
                               [(dec (count ys)) (dec (count xs))])]
                 (when (or (<= f-stat 0.0)) (f/domain-error! :div0))
                 (val/number (* 2.0 (- 1.0 (f-cdf f-stat d1 d2))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; Beta distribution

(defn- beta-cdf ^double [^double x ^double a ^double b]
  (cond (<= x 0.0) 0.0
        (>= x 1.0) 1.0
        :else (betai a b x)))

(f/register! "BETADIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     lo (if (> (count args) 3) (f/num! (nth args 3)) 0.0)
                     hi (if (> (count args) 4) (f/num! (nth args 4)) 1.0)]
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (when (<= hi lo) (f/domain-error! :num))
                 (val/number (beta-cdf (/ (- x lo) (- hi lo)) a b))))
             :arity [3 5])

(f/register! "BETA.DIST"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     cum? (f/bool! (nth args 3))
                     lo (if (> (count args) 4) (f/num! (nth args 4)) 0.0)
                     hi (if (> (count args) 5) (f/num! (nth args 5)) 1.0)]
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (when (<= hi lo) (f/domain-error! :num))
                 (let [xs (/ (- x lo) (- hi lo))]
                   (val/number
                    (if cum?
                      (beta-cdf xs a b)
                      (if (or (< xs 0.0) (> xs 1.0))
                        0.0
                        (/ (* (Math/pow xs (dec a)) (Math/pow (- 1.0 xs) (dec b)))
                           (* (- hi lo)
                              (Math/exp (+ (log-gamma a)
                                           (log-gamma b)
                                           (- (log-gamma (+ a b)))))))))))))
             :arity [4 6])

(f/register! "BETAINV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     lo (if (> (count args) 3) (f/num! (nth args 3)) 0.0)
                     hi (if (> (count args) 4) (f/num! (nth args 4)) 1.0)]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number
                  (+ lo (* (- hi lo)
                           (invert-monotone #(beta-cdf % a b) 0.0 1.0 p))))))
             :arity [3 5])

(f/register! "BETA.INV"
             ^{:scalar? true}
             (fn [args]
               (let [p (f/num! (nth args 0))
                     a (f/num! (nth args 1))
                     b (f/num! (nth args 2))
                     lo (if (> (count args) 3) (f/num! (nth args 3)) 0.0)
                     hi (if (> (count args) 4) (f/num! (nth args 4)) 1.0)]
                 (when (or (<= p 0.0) (>= p 1.0)) (f/domain-error! :num))
                 (when (or (<= a 0.0) (<= b 0.0)) (f/domain-error! :num))
                 (val/number
                  (+ lo (* (- hi lo)
                           (invert-monotone #(beta-cdf % a b) 0.0 1.0 p))))))
             :arity [3 5])

;; ---------------------------------------------------------------------------
;; PROB — Σ prob_range[i] for x_range[i] in [lo, hi]

(f/register! "PROB"
             (fn [args]
               (let [xs (f/collect-finite-numerics [(nth args 0)])
                     ps (f/collect-finite-numerics [(nth args 1)])
                     lo (f/num! (nth args 2))
                     hi (if (> (count args) 3) (f/num! (nth args 3)) lo)]
                 (when (not= (count xs) (count ps)) (f/domain-error! :na))
                 (when (some #(< (double %) 0.0) ps) (f/domain-error! :num))
                 (let [total (reduce + 0.0 ps)]
                   (when (> (Math/abs (- total 1.0)) 1.0e-9) (f/domain-error! :num))
                   (val/number (reduce + 0.0
                                       (map (fn [x p] (if (<= lo x hi) p 0.0))
                                            xs ps))))))
             :arity [3 4])

;; ---------------------------------------------------------------------------
;; LINEST / LOGEST — return a minimal scalar (slope) for now. Excel's
;; full LINEST is array-returning; a proper implementation needs the
;; evaluator's array-aware path. Returning the slope (and for LOGEST
;; the growth factor m = exp(b1)) is enough for typical scalar use.

(f/register! "LINEST"
             (fn [args]
               (let [ys (f/collect-finite-numerics [(nth args 0)])
                     xs (if (> (count args) 1)
                          (f/collect-finite-numerics [(nth args 1)])
                          (vec (range 1 (inc (count ys)))))]
                 (when (not= (count xs) (count ys)) (f/domain-error! :na))
                 (let [[m _] (slope-intercept xs ys)]
                   (val/number m))))
             :arity [1 4])

(f/register! "LOGEST"
             (fn [args]
               (let [ys (f/collect-finite-numerics [(nth args 0)])
                     xs (if (> (count args) 1)
                          (f/collect-finite-numerics [(nth args 1)])
                          (vec (range 1 (inc (count ys)))))]
                 (when (not= (count xs) (count ys)) (f/domain-error! :na))
                 (when (some #(<= (double %) 0.0) ys) (f/domain-error! :num))
                 (let [lys (mapv #(Math/log (double %)) ys)
                       [m _] (slope-intercept xs lys)]
                   (val/number (Math/exp m)))))
             :arity [1 4])

;; ---------------------------------------------------------------------------
;; FREQUENCY lives in math.cljc (already registered there as an array-aware
;; helper). We don't re-register it here.
