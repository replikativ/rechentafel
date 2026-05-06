(ns rechentafel.oracle.fn.financial-poi-test
  "Cross-check financial functions against POI. Many financial fns are either
  absent in POI (treated as NotImplemented) or implemented but with differing
  details; our oracle skips POI-not-implemented automatically."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))

(defn- area [rows]
  (let [wrap (fn [x] (if (number? x) (n x) x))]
    {:t :area
     :values (mapv #(mapv wrap %) rows)
     :r0 0 :c0 0
     :r1 (dec (count rows)) :c1 (dec (count (first rows)))}))

(defn- check! [cases]
  (let [{:keys [failures] :as r} (po/check-many cases)]
    (doseq [fail failures] (is false (po/report fail)))
    (is (zero? (count failures))
        (str "passed " (:passed r) "/" (:total r)))))

(deftest annuity
  (check! [{:fname "FV"   :args [(n 0.06) (n 10) (n -200) (n -500) (n 1)]}
           {:fname "FV"   :args [(n 0.005) (n 12) (n -100) (n 0) (n 0)]}
           {:fname "PV"   :args [(n 0.08) (n 20) (n 500) (n 0) (n 0)]}
           {:fname "PV"   :args [(n 0.01) (n 60) (n 100) (n 0) (n 1)]}
           {:fname "PMT"  :args [(n 0.08) (n 10) (n 10000) (n 0) (n 0)]}
           {:fname "PMT"  :args [(n 0.005) (n 360) (n 100000) (n 0) (n 0)]}
           {:fname "NPER" :args [(n 0.08) (n -1000) (n 5000) (n 0) (n 0)]}
           {:fname "NPER" :args [(n 0.005) (n -500) (n 10000) (n 0) (n 0)]}
           {:fname "IPMT" :args [(n 0.08) (n 1) (n 10) (n 10000)]}
           {:fname "IPMT" :args [(n 0.08) (n 5) (n 10) (n 10000)]}
           {:fname "PPMT" :args [(n 0.08) (n 1) (n 10) (n 10000)]}
           {:fname "PPMT" :args [(n 0.08) (n 5) (n 10) (n 10000)]}
           {:fname "ISPMT" :args [(n 0.08) (n 5) (n 10) (n 10000)]}]))

(deftest rate-family
  (check! [{:fname "RATE" :args [(n 10) (n -2000) (n 15000) (n 0) (n 0) (n 0.1)]}
           {:fname "EFFECT"  :args [(n 0.0525) (n 4)]}
           {:fname "NOMINAL" :args [(n 0.053543) (n 4)]}]))

(deftest npv-irr
  (let [flows (area [[-1000 300 400 500 200]])
        all-neg (area [[-1000 -300]])]
    (check! [{:fname "NPV" :args [(n 0.1) (n 100) (n 200) (n 300)]}
             {:fname "NPV" :args [(n 0.08) flows]}
             {:fname "IRR" :args [flows]}
             ;; IRR with guess
             {:fname "IRR" :args [flows (n 0.1)]}
             ;; MIRR
             {:fname "MIRR" :args [flows (n 0.1) (n 0.12)]}
             ;; Cumulative payments/interest
             {:fname "CUMIPMT"  :args [(n 0.08) (n 10) (n 10000) (n 1) (n 5) (n 0)]}
             {:fname "CUMPRINC" :args [(n 0.08) (n 10) (n 10000) (n 1) (n 5) (n 0)]}])))

(deftest depreciation
  (check! [{:fname "SLN"  :args [(n 30000) (n 7500) (n 10)]}
           {:fname "SYD"  :args [(n 30000) (n 7500) (n 10) (n 1)]}
           {:fname "DB"   :args [(n 1000000) (n 100000) (n 6) (n 1) (n 12)]}
           {:fname "DB"   :args [(n 1000000) (n 100000) (n 6) (n 2) (n 12)]}
           {:fname "DDB"  :args [(n 2400) (n 300) (n 10) (n 1) (n 2)]}
           {:fname "DDB"  :args [(n 2400) (n 300) (n 10) (n 5) (n 2)]}
           {:fname "VDB"  :args [(n 2400) (n 300) (n 10) (n 0) (n 1) (n 2) v/FALSE]}
           {:fname "VDB"  :args [(n 2400) (n 300) (n 10) (n 0) (n 5) (n 2) v/FALSE]}]))

;; TBILL/DISC/INTRATE/RECEIVED/ACCRINTM: we have real implementations but these
;; are ATP fns that POI treats as NotImplemented unless another namespace has
;; patched AnalysisToolPak (e.g. the old `rechentafel.udf_finance`). The stubs below
;; (coupon-dates, bond-pricing, odd-period-bond, french-depreciation) are #N/A
;; placeholders in our v2 registry and intentionally skipped from POI checks.

(deftest scheduled-and-conversion
  (check! [{:fname "DOLLARDE" :args [(n 1.02) (n 16)]}
           {:fname "DOLLARFR" :args [(n 1.125) (n 16)]}]))
