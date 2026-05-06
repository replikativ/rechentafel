(ns rechentafel.oracle.fn.stats-poi-test
  "Cross-check statistics functions against POI."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- area
  [rows]
  (let [wrap (fn [x]
               (cond (number? x)  (n x)
                     (string? x)  (s x)
                     (boolean? x) (v/boolean-v x)
                     :else        x))]
    {:t :area
     :values (mapv #(mapv wrap %) rows)
     :r0 0 :c0 0
     :r1 (dec (count rows)) :c1 (dec (count (first rows)))}))

(defn- check! [cases]
  (let [{:keys [failures] :as r} (po/check-many cases)]
    (doseq [fail failures] (is false (po/report fail)))
    (is (zero? (count failures))
        (str "passed " (:passed r) "/" (:total r)))))

(def ^:private nums1 (area [[1 2 3 4 5 6 7 8 9 10]]))
(def ^:private nums2 (area [[2 4 6 8 10]]))
(def ^:private xs    (area [[1 2 3 4 5]]))
(def ^:private ys    (area [[2 4 5 4 5]]))

(deftest basics
  (check! [{:fname "AVERAGE" :args [nums1]}
           {:fname "AVERAGE" :args [(n 1) (n 2) (n 3)]}
           {:fname "AVERAGEA" :args [nums1]}
           {:fname "COUNT"   :args [nums1]}
           {:fname "COUNTA"  :args [nums1]}
           {:fname "COUNTBLANK" :args [(area [[1 2] [3 4]])]}
           {:fname "MAX"     :args [nums1]}
           {:fname "MIN"     :args [nums1]}
           {:fname "MAXA"    :args [nums1]}
           {:fname "MINA"    :args [nums1]}
           {:fname "SUM"     :args [nums1]}]))

(deftest dispersion
  (check! [{:fname "DEVSQ"   :args [nums1]}
           {:fname "VAR"     :args [nums1]}
           {:fname "VAR.S"   :args [nums1]}
           {:fname "VARP"    :args [nums1]}
           {:fname "VAR.P"   :args [nums1]}
           {:fname "STDEV"   :args [nums1]}
           {:fname "STDEV.S" :args [nums1]}
           {:fname "STDEVP"  :args [nums1]}
           {:fname "STDEV.P" :args [nums1]}
           {:fname "VARA"    :args [nums1]}
           {:fname "VARPA"   :args [nums1]}
           {:fname "STDEVA"  :args [nums1]}
           {:fname "STDEVPA" :args [nums1]}]))

(deftest order-stats
  (check! [{:fname "MEDIAN"  :args [nums1]}
           {:fname "MODE"    :args [(area [[1 2 2 3 4]])]}
           {:fname "MODE.SNGL" :args [(area [[1 2 2 3 4]])]}
           {:fname "LARGE"   :args [nums1 (n 2)]}
           {:fname "SMALL"   :args [nums1 (n 2)]}
           {:fname "PERCENTILE"     :args [nums1 (n 0.5)]}
           {:fname "PERCENTILE.INC" :args [nums1 (n 0.25)]}
           {:fname "PERCENTILE.EXC" :args [nums1 (n 0.5)]}
           {:fname "QUARTILE"       :args [nums1 (n 1)]}
           {:fname "QUARTILE.INC"   :args [nums1 (n 2)]}
           {:fname "QUARTILE.EXC"   :args [nums1 (n 2)]}
           {:fname "PERCENTRANK"     :args [nums1 (n 5)]}
           {:fname "PERCENTRANK.INC" :args [nums1 (n 5)]}
           {:fname "PERCENTRANK.EXC" :args [nums1 (n 5)]}
           {:fname "RANK"     :args [(n 5) nums1]}
           {:fname "RANK.EQ"  :args [(n 5) nums1]}
           {:fname "RANK.AVG" :args [(n 5) nums1]}]))

(deftest means-and-shape
  (check! [{:fname "GEOMEAN" :args [nums1]}
           {:fname "HARMEAN" :args [nums1]}
           {:fname "TRIMMEAN" :args [nums1 (n 0.2)]}
           {:fname "SKEW"    :args [nums1]}
           {:fname "SKEW.P"  :args [nums1]}
           {:fname "KURT"    :args [nums1]}]))

(deftest correlation
  (check! [{:fname "CORREL"       :args [xs ys]}
           {:fname "PEARSON"      :args [xs ys]}
           {:fname "RSQ"          :args [xs ys]}
           {:fname "COVAR"        :args [xs ys]}
           {:fname "COVARIANCE.P" :args [xs ys]}
           {:fname "COVARIANCE.S" :args [xs ys]}
           {:fname "SLOPE"        :args [ys xs]}
           {:fname "INTERCEPT"    :args [ys xs]}
           {:fname "FORECAST"         :args [(n 6) ys xs]}
           {:fname "FORECAST.LINEAR"  :args [(n 6) ys xs]}
           {:fname "STEYX"            :args [ys xs]}
           {:fname "STANDARDIZE"      :args [(n 42) (n 40) (n 1.5)]}
           {:fname "FISHER"           :args [(n 0.75)]}
           {:fname "FISHERINV"        :args [(n 1)]}]))

(deftest conditional-aggregates
  (let [data  (area [[10 20 30 40 50]])
        crit  (area [["a" "b" "a" "b" "a"]])]
    (check! [{:fname "COUNTIF"   :args [crit (s "a")]}
             {:fname "SUMIF"     :args [crit (s "a") data]}
             {:fname "AVERAGEIF" :args [crit (s "a") data]}
             {:fname "COUNTIFS"  :args [crit (s "a")]}
             {:fname "SUMIFS"    :args [data crit (s "a")]}
             {:fname "AVERAGEIFS" :args [data crit (s "a")]}
             {:fname "MAXIFS"    :args [data crit (s "a")]}
             {:fname "MINIFS"    :args [data crit (s "a")]}])))

(deftest normal-distributions
  (check! [{:fname "NORMDIST"    :args [(n 1) (n 0) (n 1) v/FALSE]}
           {:fname "NORMDIST"    :args [(n 1) (n 0) (n 1) v/TRUE]}
           {:fname "NORM.DIST"   :args [(n 1) (n 0) (n 1) v/TRUE]}
           {:fname "NORMSDIST"   :args [(n 1)]}
           ;; POI's NORM.S.DIST returns #VALUE! regardless of args — POI bug;
           ;; coverage comes from NORMSDIST.

           {:fname "NORMINV"     :args [(n 0.5) (n 0) (n 1)]}
           {:fname "NORM.INV"    :args [(n 0.5) (n 0) (n 1)]}
           {:fname "NORMSINV"    :args [(n 0.975)]}
           {:fname "NORM.S.INV"  :args [(n 0.975)]}
           {:fname "CONFIDENCE"      :args [(n 0.05) (n 2) (n 100)]}
           {:fname "CONFIDENCE.NORM" :args [(n 0.05) (n 2) (n 100)]}]))

(deftest lognormal-exp-weibull
  (check! [{:fname "LOGNORMDIST"  :args [(n 4) (n 3.5) (n 1.2)]}
           {:fname "LOGNORM.DIST" :args [(n 4) (n 3.5) (n 1.2) v/TRUE]}
           {:fname "LOGINV"       :args [(n 0.5) (n 3.5) (n 1.2)]}
           {:fname "LOGNORM.INV"  :args [(n 0.5) (n 3.5) (n 1.2)]}
           {:fname "EXPONDIST"    :args [(n 0.2) (n 10) v/TRUE]}
           {:fname "EXPON.DIST"   :args [(n 0.2) (n 10) v/TRUE]}
           {:fname "WEIBULL"      :args [(n 105) (n 20) (n 100) v/TRUE]}
           {:fname "WEIBULL.DIST" :args [(n 105) (n 20) (n 100) v/TRUE]}]))

(deftest discrete-distributions
  (check! [{:fname "POISSON"       :args [(n 2) (n 5) v/TRUE]}
           {:fname "POISSON.DIST"  :args [(n 2) (n 5) v/FALSE]}
           {:fname "BINOMDIST"     :args [(n 6) (n 10) (n 0.5) v/FALSE]}
           {:fname "BINOM.DIST"    :args [(n 6) (n 10) (n 0.5) v/TRUE]}
           {:fname "CRITBINOM"     :args [(n 10) (n 0.5) (n 0.75)]}
           {:fname "BINOM.INV"     :args [(n 10) (n 0.5) (n 0.75)]}
           {:fname "NEGBINOMDIST"  :args [(n 10) (n 5) (n 0.25)]}
           {:fname "NEGBINOM.DIST" :args [(n 10) (n 5) (n 0.25) v/FALSE]}
           {:fname "HYPGEOMDIST"   :args [(n 1) (n 4) (n 8) (n 20)]}
           {:fname "HYPGEOM.DIST"  :args [(n 1) (n 4) (n 8) (n 20) v/FALSE]}]))

(deftest gamma-beta
  (check! [{:fname "GAMMALN"         :args [(n 4.5)]}
           {:fname "GAMMALN.PRECISE" :args [(n 4.5)]}
           {:fname "GAMMA"           :args [(n 2.5)]}
           {:fname "GAMMADIST"       :args [(n 10) (n 9) (n 2) v/TRUE]}
           {:fname "GAMMA.DIST"      :args [(n 10) (n 9) (n 2) v/FALSE]}
           {:fname "GAMMAINV"        :args [(n 0.5) (n 9) (n 2)]}
           {:fname "GAMMA.INV"       :args [(n 0.5) (n 9) (n 2)]}
           {:fname "BETADIST"        :args [(n 2) (n 8) (n 10) (n 1) (n 3)]}
           {:fname "BETA.DIST"       :args [(n 2) (n 8) (n 10) v/TRUE (n 1) (n 3)]}
           {:fname "BETAINV"         :args [(n 0.6) (n 8) (n 10) (n 1) (n 3)]}
           {:fname "BETA.INV"        :args [(n 0.6) (n 8) (n 10) (n 1) (n 3)]}]))

(deftest chi-t-f
  (check! [{:fname "CHIDIST"      :args [(n 18.307) (n 10)]}
           {:fname "CHISQ.DIST"   :args [(n 1) (n 2) v/TRUE]}
           {:fname "CHISQ.DIST.RT" :args [(n 18.307) (n 10)]}
           {:fname "CHIINV"       :args [(n 0.05) (n 10)]}
           {:fname "CHISQ.INV"    :args [(n 0.5) (n 10)]}
           {:fname "CHISQ.INV.RT" :args [(n 0.05) (n 10)]}
           {:fname "TDIST"        :args [(n 2) (n 10) (n 2)]}
           {:fname "T.DIST"       :args [(n 2) (n 10) v/TRUE]}
           {:fname "T.DIST.RT"    :args [(n 2) (n 10)]}
           {:fname "T.DIST.2T"    :args [(n 2) (n 10)]}
           {:fname "TINV"         :args [(n 0.05) (n 10)]}
           {:fname "T.INV"        :args [(n 0.75) (n 10)]}
           {:fname "T.INV.2T"     :args [(n 0.05) (n 10)]}
           {:fname "FDIST"        :args [(n 2) (n 4) (n 5)]}
           {:fname "F.DIST"       :args [(n 2) (n 4) (n 5) v/TRUE]}
           {:fname "F.DIST.RT"    :args [(n 2) (n 4) (n 5)]}
           {:fname "FINV"         :args [(n 0.05) (n 4) (n 5)]}
           {:fname "F.INV"        :args [(n 0.25) (n 4) (n 5)]}
           {:fname "F.INV.RT"     :args [(n 0.05) (n 4) (n 5)]}]))
