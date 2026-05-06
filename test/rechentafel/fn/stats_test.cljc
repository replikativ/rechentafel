(ns rechentafel.fn.stats-test
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.stats]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- area
  "Build an area value from a row-of-row matrix of scalars."
  [rows]
  {:t :area :r0 0 :c0 0
   :r1 (dec (count rows)) :c1 (dec (count (first rows)))
   :values (mapv (fn [row]
                   (mapv (fn [x]
                           (cond (number? x)  (n x)
                                 (string? x)  (s x)
                                 (boolean? x) (v/boolean-v x)
                                 :else x))
                         row))
                 rows)})

(defn- approx [expected actual tol]
  (< (Math/abs (- (double expected) (double (:v actual))))
     tol))

;; ---------------------------------------------------------------------------
;; Basic aggregates

(deftest basic-aggregates
  (is (= (n 2)   (f/call "AVERAGE" [(n 1) (n 2) (n 3)])))
  (is (= (n 3)   (f/call "MAX"     [(n 1) (n 2) (n 3)])))
  (is (= (n 1)   (f/call "MIN"     [(n 1) (n 2) (n 3)])))
  (is (= (n 2.5) (f/call "MEDIAN"  [(n 1) (n 2) (n 3) (n 4)])))
  (is (= (n 2.0) (f/call "COUNT"   [(n 1) (n 2) (s "foo")])))
  (is (= (n 3.0) (f/call "COUNTA"  [(n 1) (n 2) (s "foo")])))
  (is (= (n 2.0) (f/call "COUNTBLANK" [(area [[v/BLANK 1 v/BLANK]])])))
  (is (= v/ERR-DIV0 (f/call "AVERAGE" [v/BLANK]))))

(deftest variance-stdev
  (let [args [(n 2) (n 4) (n 4) (n 4) (n 5) (n 5) (n 7) (n 9)]]
    (is (approx 4.571428 (f/call "VAR"    args) 1e-5))
    (is (approx 4.0      (f/call "VARP"   args) 1e-9))
    (is (approx 2.138089 (f/call "STDEV"  args) 1e-5))
    (is (approx 2.0      (f/call "STDEVP" args) 1e-9))))

(deftest geomean-harmean-devsq
  (is (approx 4.0      (f/call "GEOMEAN" [(n 2) (n 4) (n 8)]) 1e-9))
  (is (approx (/ 12.0 7.0) (f/call "HARMEAN" [(n 1) (n 2) (n 4)]) 1e-9))
  (is (approx 2.0      (f/call "DEVSQ"   [(n 1) (n 2) (n 3)])  1e-9)))

;; ---------------------------------------------------------------------------
;; Ranking / percentile

(deftest large-small-percentile
  (let [a (area [[3 1 5 2 4]])]
    (is (= (n 4.0) (f/call "LARGE" [a (n 2)])))
    (is (= (n 2.0) (f/call "SMALL" [a (n 2)])))
    (is (= (n 3.0) (f/call "PERCENTILE" [a (n 0.5)])))
    (is (= (n 2.0) (f/call "QUARTILE"   [a (n 1)])))))

(deftest rank-fns
  (let [a (area [[3 1 5 2 4]])]
    (is (= (n 2.0) (f/call "RANK" [(n 4) a])))   ;; desc: 5,4,3,2,1 → 2
    (is (= (n 4.0) (f/call "RANK" [(n 4) a (n 1)]))) ;; asc: 1,2,3,4,5 → 4
    (is (= (n 3.0) (f/call "RANK.EQ" [(n 3) a])))))

(deftest trimmean
  (let [a (area [[1 2 3 4 5 6 7 8 9 10]])]
    ;; 0.2 → trim 1 from each end → mean of 2..9 = 5.5
    (is (= (n 5.5) (f/call "TRIMMEAN" [a (n 0.2)])))))

;; ---------------------------------------------------------------------------
;; Regression

(deftest regression
  (let [xs (area [[1 2 3 4 5]])
        ys (area [[2 4 6 8 10]])]
    (is (approx 2.0  (f/call "SLOPE"     [ys xs]) 1e-9))
    (is (approx 0.0  (f/call "INTERCEPT" [ys xs]) 1e-9))
    (is (approx 1.0  (f/call "CORREL"    [xs ys]) 1e-9))
    (is (approx 1.0  (f/call "RSQ"       [xs ys]) 1e-9))
    (is (approx 4.0  (f/call "COVAR"     [xs ys]) 1e-9))
    (is (approx 20.0 (f/call "FORECAST"  [(n 10) ys xs]) 1e-9))))

;; ---------------------------------------------------------------------------
;; Conditional aggregates

(deftest countif-sumif
  (let [data   (area [[1 2 3 4 5 6 7 8 9 10]])
        labels (area [["apple" "banana" "apple" "cherry" "apple"
                       "banana" "apple" "cherry" "banana" "apple"]])]
    (is (= (n 1.0)  (f/call "COUNTIF" [data (n 5)])))
    (is (= (n 6.0)  (f/call "COUNTIF" [data (s ">=5")])))
    (is (= (n 5.0)  (f/call "COUNTIF" [labels (s "apple")])))
    (is (= (n 5.0)  (f/call "COUNTIF" [labels (s "a*")])))
    (is (= (n 26.0) (f/call "SUMIF"   [labels (s "apple") data])))
    (is (= (n 40.0) (f/call "SUMIF"   [data (s ">5")])))
    (is (= (n 5.2)  (f/call "AVERAGEIF" [labels (s "apple") data])))))

(deftest countifs-sumifs
  (let [data   (area [[1 2 3 4 5 6 7 8 9 10]])
        labels (area [["apple" "banana" "apple" "cherry" "apple"
                       "banana" "apple" "cherry" "banana" "apple"]])]
    (is (= (n 3.0)  (f/call "COUNTIFS" [labels (s "apple") data (s ">3")])))
    (is (= (n 22.0) (f/call "SUMIFS"   [data labels (s "apple") data (s ">3")])))
    (is (= (n 10.0) (f/call "MAXIFS"   [data labels (s "apple")])))
    (is (= (n 1.0)  (f/call "MINIFS"   [data labels (s "apple")])))))

;; ---------------------------------------------------------------------------
;; Distributions

(deftest normal-distribution
  (is (approx 0.975 (f/call "NORMSDIST" [(n 1.96)]) 1e-4))
  (is (approx 1.96  (f/call "NORMSINV"  [(n 0.975)]) 1e-4))
  (is (approx 0.5   (f/call "NORMDIST"  [(n 0) (n 0) (n 1) v/TRUE])  1e-9))
  (is (approx 0.3989423 (f/call "NORMDIST" [(n 0) (n 0) (n 1) v/FALSE]) 1e-6))
  (is (= (n 1.0) (f/call "STANDARDIZE" [(n 5) (n 3) (n 2)]))))

(deftest binomial-poisson
  (is (approx 0.1171875 (f/call "BINOMDIST" [(n 3) (n 10) (n 0.5) v/FALSE]) 1e-6))
  (is (approx 0.171875  (f/call "BINOMDIST" [(n 3) (n 10) (n 0.5) v/TRUE])  1e-5))
  (is (approx 0.185     (f/call "POISSON"   [(n 2) (n 3.5) v/FALSE]) 1e-3))
  (is (approx 0.321     (f/call "POISSON"   [(n 2) (n 3.5) v/TRUE])  1e-3)))

(deftest chi-t-f
  (is (approx 0.05 (f/call "CHIDIST" [(n 3.84) (n 1)]) 1e-3))
  (is (approx 3.84 (f/call "CHIINV"  [(n 0.05) (n 1)]) 1e-2))
  (is (approx 0.053 (f/call "TDIST" [(n 1.96) (n 100) (n 2)]) 1e-3))
  (is (approx 1.984 (f/call "TINV"  [(n 0.05) (n 100)]) 1e-2))
  (is (approx 0.0655 (f/call "FDIST" [(n 3) (n 5) (n 10)]) 1e-3))
  (is (approx 3.326  (f/call "FINV"  [(n 0.05) (n 5) (n 10)]) 1e-2)))

(deftest exponential-weibull
  (is (approx 0.632 (f/call "EXPONDIST" [(n 1) (n 1) v/TRUE])  1e-3))
  (is (approx 0.368 (f/call "EXPONDIST" [(n 1) (n 1) v/FALSE]) 1e-3))
  (is (approx 0.632 (f/call "WEIBULL"  [(n 1) (n 1) (n 1) v/TRUE]) 1e-3)))

(deftest transforms
  (is (approx 0.5493 (f/call "FISHER"    [(n 0.5)]) 1e-4))
  (is (approx 0.5    (f/call "FISHERINV" [(n 0.5493)]) 1e-3)))
