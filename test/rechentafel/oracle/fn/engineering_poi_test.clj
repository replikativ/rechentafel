(ns rechentafel.oracle.fn.engineering-poi-test
  "Cross-check engineering functions against POI."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- check! [cases]
  (let [{:keys [failures] :as r} (po/check-many cases)]
    (doseq [fail failures] (is false (po/report fail)))
    (is (zero? (count failures))
        (str "passed " (:passed r) "/" (:total r)))))

(deftest base-conversions
  (check! [{:fname "BIN2DEC" :args [(s "1010")]}
           {:fname "BIN2DEC" :args [(s "1111111111")]}    ;; 1023
           {:fname "OCT2DEC" :args [(s "17")]}             ;; 15
           {:fname "OCT2DEC" :args [(s "777")]}            ;; 511
           {:fname "HEX2DEC" :args [(s "FF")]}             ;; 255
           {:fname "HEX2DEC" :args [(s "A5")]}             ;; 165
           {:fname "DEC2BIN" :args [(n 10)]}
           ;; POI doesn't pad with `places`; our impl does per Excel docs.
           {:fname "DEC2OCT" :args [(n 15)]}
           {:fname "DEC2HEX" :args [(n 255)]}
           ;; Skip `places` for DEC2HEX too — same POI pad bug.
           {:fname "BIN2OCT" :args [(s "1010")]}
           {:fname "BIN2HEX" :args [(s "1010")]}
           {:fname "OCT2BIN" :args [(s "17")]}
           {:fname "OCT2HEX" :args [(s "17")]}
           {:fname "HEX2BIN" :args [(s "A")]}
           {:fname "HEX2OCT" :args [(s "FF")]}]))

(deftest delta-gestep
  ;; POI requires 2 args for DELTA/GESTEP (POI bug — Excel allows 1).
  (check! [{:fname "DELTA"  :args [(n 1) (n 1)]}
           {:fname "DELTA"  :args [(n 1) (n 2)]}
           {:fname "GESTEP" :args [(n 5) (n 4)]}
           {:fname "GESTEP" :args [(n 3) (n 4)]}]))

(deftest erf-family
  (check! [{:fname "ERF"          :args [(n 0)]}
           {:fname "ERF"          :args [(n 1)]}
           {:fname "ERF"          :args [(n 2)]}
           {:fname "ERFC"         :args [(n 1)]}
           {:fname "ERF.PRECISE"  :args [(n 1)]}
           {:fname "ERFC.PRECISE" :args [(n 1)]}]))

(deftest complex-fns
  ;; POI's IMREAL/IMAGINARY return StringEval (POI bug — Excel returns number).
  (check! [{:fname "COMPLEX"   :args [(n 3) (n 4)]}
           {:fname "COMPLEX"   :args [(n 3) (n 4) (s "j")]}
           {:fname "IMABS"     :args [(s "3+4i")]}
           {:fname "IMARGUMENT" :args [(s "1+1i")]}
           {:fname "IMCONJUGATE" :args [(s "3+4i")]}
           {:fname "IMSUM"     :args [(s "1+1i") (s "2+2i")]}
           {:fname "IMSUB"     :args [(s "5+7i") (s "2+3i")]}
           {:fname "IMPRODUCT" :args [(s "1+2i") (s "3+4i")]}
           {:fname "IMDIV"     :args [(s "-1+5i") (s "3+4i")]}
           {:fname "IMEXP"     :args [(s "1+0i")]}
           {:fname "IMLN"      :args [(s "2+0i")]}
           {:fname "IMSQRT"    :args [(s "1+0i")]}
           {:fname "IMCOS"     :args [(s "1+0i")]}
           {:fname "IMSIN"     :args [(s "1+0i")]}]))
