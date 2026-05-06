(ns rechentafel.oracle.fn.math-poi-test
  "Cross-check every math function against Apache POI's formula evaluator.
  POI is the oracle: for each case we build the equivalent formula string,
  evaluate it through POI, and compare against our `(f/call ...)` result."
  (:require [clojure.test :refer [deftest is testing]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))
(defn- area [rows]
  {:t :area
   :values (mapv #(mapv v/number %) rows)
   :r0 0 :c0 0 :r1 (dec (count rows)) :c1 (dec (count (first rows)))})

(defn- check! [cases]
  (let [{:keys [failures] :as r} (po/check-many cases)]
    (doseq [fail failures] (is false (po/report fail)))
    (is (zero? (count failures))
        (str "passed " (:passed r) "/" (:total r)))))

(deftest unary-arithmetic
  (check! [{:fname "ABS"    :args [(n -7.5)]}
           {:fname "ABS"    :args [(n 0)]}
           {:fname "SIGN"   :args [(n -3)]}
           {:fname "SIGN"   :args [(n 0)]}
           {:fname "SIGN"   :args [(n 5)]}
           {:fname "INT"    :args [(n 3.7)]}
           {:fname "INT"    :args [(n -3.2)]}       ;; floor: -4
           {:fname "TRUNC"  :args [(n 3.7)]}
           {:fname "TRUNC"  :args [(n -3.7)]}
           {:fname "TRUNC"  :args [(n 3.789) (n 2)]}
           {:fname "EVEN"   :args [(n 3)]}
           {:fname "EVEN"   :args [(n -1)]}
           {:fname "ODD"    :args [(n 2)]}
           {:fname "ODD"    :args [(n -2)]}
           {:fname "SQRT"   :args [(n 16)]}
           {:fname "SQRTPI" :args [(n 9)]}
           {:fname "EXP"    :args [(n 1)]}
           {:fname "LN"     :args [(n (Math/exp 1))]}
           {:fname "LOG"    :args [(n 100)]}            ;; default base 10
           {:fname "LOG"    :args [(n 1000) (n 10)]}
           {:fname "LOG10"  :args [(n 1000)]}]))

(deftest trig
  (check! [{:fname "SIN"    :args [(n 0)]}
           {:fname "SIN"    :args [(n (/ Math/PI 2))]}
           {:fname "COS"    :args [(n 0)]}
           {:fname "COS"    :args [(n Math/PI)]}
           {:fname "TAN"    :args [(n 0)]}
           {:fname "ASIN"   :args [(n 0.5)]}
           {:fname "ACOS"   :args [(n 0.5)]}
           {:fname "ATAN"   :args [(n 1)]}
           {:fname "ATAN2"  :args [(n 1) (n 1)]}
           {:fname "SINH"   :args [(n 1)]}
           {:fname "COSH"   :args [(n 1)]}
           {:fname "TANH"   :args [(n 1)]}
           {:fname "ASINH"  :args [(n 0.5)]}
           {:fname "ACOSH"  :args [(n 2)]}
           {:fname "ATANH"  :args [(n 0.5)]}
           {:fname "PI"     :args []}
           {:fname "DEGREES" :args [(n Math/PI)]}
           {:fname "RADIANS" :args [(n 180)]}]))

(deftest binary-arithmetic
  (check! [{:fname "POWER"    :args [(n 2) (n 10)]}
           {:fname "POWER"    :args [(n 9) (n 0.5)]}
           {:fname "MOD"      :args [(n 7) (n 3)]}
           {:fname "MOD"      :args [(n -5) (n 3)]}   ;; sign-of-divisor
           {:fname "QUOTIENT" :args [(n 7) (n 3)]}
           {:fname "QUOTIENT" :args [(n -7) (n 3)]}]))

(deftest rounding
  (check! [{:fname "ROUND"          :args [(n 3.14159) (n 2)]}
           {:fname "ROUND"          :args [(n 3.5) (n 0)]}
           {:fname "ROUND"          :args [(n -3.5) (n 0)]}
           {:fname "ROUNDUP"        :args [(n 3.2) (n 0)]}
           {:fname "ROUNDUP"        :args [(n -3.2) (n 0)]}
           {:fname "ROUNDDOWN"      :args [(n 3.9) (n 0)]}
           {:fname "ROUNDDOWN"      :args [(n -3.9) (n 0)]}
           {:fname "CEILING"        :args [(n 2.5) (n 1)]}
           {:fname "CEILING"        :args [(n -4.3) (n -1)]}
           {:fname "FLOOR"          :args [(n 2.5) (n 1)]}
           {:fname "FLOOR"          :args [(n -4.3) (n -1)]}
           {:fname "CEILING.MATH"   :args [(n 2.5)]}
           {:fname "CEILING.MATH"   :args [(n -2.5)]}
           {:fname "FLOOR.MATH"     :args [(n 2.5)]}
           {:fname "FLOOR.MATH"     :args [(n -2.5)]}
           {:fname "CEILING.PRECISE" :args [(n 2.5)]}
           {:fname "FLOOR.PRECISE"  :args [(n 2.5)]}
           {:fname "MROUND"         :args [(n 12) (n 5)]}
           {:fname "MROUND"         :args [(n 7.5) (n 2.5)]}]))

(deftest combinatorics
  (check! [{:fname "FACT"         :args [(n 5)]}
           {:fname "FACT"         :args [(n 0)]}
           {:fname "FACTDOUBLE"   :args [(n 5)]}
           {:fname "FACTDOUBLE"   :args [(n 6)]}
           {:fname "COMBIN"       :args [(n 5) (n 2)]}
           {:fname "COMBIN"       :args [(n 10) (n 3)]}
           {:fname "COMBINA"      :args [(n 5) (n 2)]}
           {:fname "PERMUT"       :args [(n 5) (n 2)]}
           {:fname "PERMUTATIONA" :args [(n 5) (n 2)]}
           {:fname "GCD"          :args [(n 12) (n 18)]}
           {:fname "GCD"          :args [(n 24) (n 36) (n 48)]}
           {:fname "LCM"          :args [(n 4) (n 6)]}
           {:fname "LCM"          :args [(n 3) (n 4) (n 5)]}
           {:fname "MULTINOMIAL"  :args [(n 2) (n 3) (n 4)]}]))

(deftest aggregate-sums
  (check! [{:fname "SUM"        :args [(n 1) (n 2) (n 3)]}
           {:fname "SUM"        :args [(area [[1 2 3] [4 5 6]])]}
           {:fname "SUMSQ"      :args [(n 1) (n 2) (n 3)]}
           {:fname "SUMSQ"      :args [(area [[3 4]])]}
           {:fname "PRODUCT"    :args [(n 2) (n 3) (n 4)]}
           {:fname "PRODUCT"    :args [(area [[2 3] [4 5]])]}
           {:fname "SUMPRODUCT" :args [(area [[1 2 3]]) (area [[4 5 6]])]}
           {:fname "SUMX2MY2"   :args [(area [[1 2 3]]) (area [[4 5 6]])]}
           {:fname "SUMX2PY2"   :args [(area [[1 2 3]]) (area [[4 5 6]])]}
           {:fname "SUMXMY2"    :args [(area [[1 2 3]]) (area [[4 5 6]])]}
           {:fname "SERIESSUM"  :args [(n 2) (n 0) (n 1) (area [[1 1 1 1]])]}]))

(deftest roman
  ;; POI requires ROMAN's 2-arg form; our impl also supports 1-arg per Excel.
  (check! [{:fname "ROMAN"   :args [(n 1994) (n 0)]}
           {:fname "ROMAN"   :args [(n 1) (n 0)]}
           {:fname "ROMAN"   :args [(n 3999) (n 0)]}
           {:fname "ROMAN"   :args [(n 499) (n 0)]}
           {:fname "ROMAN"   :args [(n 499) (n 4)]}])
  ;; ARABIC is a POI gap (returns NotImplemented); exercise locally.
  (is (= (n 1994) (f/call "ARABIC" [(s "MCMXCIV")])))
  (is (= (n 4)    (f/call "ARABIC" [(s "IV")])))
  (is (= (n 3)    (f/call "ARABIC" [(s "III")]))))

(deftest avedev
  (check! [{:fname "AVEDEV" :args [(n 1) (n 2) (n 3) (n 4) (n 5)]}
           {:fname "AVEDEV" :args [(area [[4 5 6 7 5 4 3]])]}]))

(deftest subtotal-agg
  (check! [{:fname "SUBTOTAL" :args [(n 9)  (area [[1 2 3 4]])]}       ;; SUM
           {:fname "SUBTOTAL" :args [(n 1)  (area [[1 2 3 4]])]}       ;; AVERAGE
           {:fname "SUBTOTAL" :args [(n 2)  (area [[1 2 3 4]])]}       ;; COUNT
           {:fname "SUBTOTAL" :args [(n 3)  (area [[1 2 3 4]])]}       ;; COUNTA
           {:fname "SUBTOTAL" :args [(n 4)  (area [[1 2 3 4]])]}       ;; MAX
           {:fname "SUBTOTAL" :args [(n 5)  (area [[1 2 3 4]])]}       ;; MIN
           {:fname "SUBTOTAL" :args [(n 6)  (area [[1 2 3 4]])]}       ;; PRODUCT
           {:fname "SUBTOTAL" :args [(n 109) (area [[1 2 3 4]])]}]))   ;; SUM (ignore-hidden)
