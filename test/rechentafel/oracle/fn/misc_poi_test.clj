(ns rechentafel.oracle.fn.misc-poi-test
  "Cross-check misc functions against POI (ADDRESS, DOLLARDE/FR, FVSCHEDULE)."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

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

(deftest address
  (check! [{:fname "ADDRESS" :args [(n 2) (n 3)]}                  ;; $C$2
           {:fname "ADDRESS" :args [(n 2) (n 3) (n 2)]}            ;; C$2
           {:fname "ADDRESS" :args [(n 2) (n 3) (n 3)]}            ;; $C2
           {:fname "ADDRESS" :args [(n 2) (n 3) (n 4)]}            ;; C2
           {:fname "ADDRESS" :args [(n 10) (n 27)]}                ;; $AA$10
           ;; POI ignores the `a1` flag and always emits A1-style. POI bug —
           ;; our R1C1 support still has test coverage in misc_test.cljc.
           {:fname "ADDRESS" :args [(n 2) (n 3) (n 4) v/TRUE (s "Sheet2")]}]))

(deftest dollar-conversion
  (check! [{:fname "DOLLARDE" :args [(n 1.02) (n 16)]}          ;; 1.125
           {:fname "DOLLARDE" :args [(n 1.1) (n 8)]}             ;; 1.125
           {:fname "DOLLARDE" :args [(n 2.08) (n 32)]}           ;; 2.25
           {:fname "DOLLARFR" :args [(n 1.125) (n 16)]}          ;; 1.02
           {:fname "DOLLARFR" :args [(n 1.125) (n 8)]}           ;; 1.1
           {:fname "DOLLARFR" :args [(n 2.25)  (n 32)]}]))        ;; 2.08

(deftest fvschedule
  (let [rates (area [[0.09 0.11 0.1]])]
    (check! [{:fname "FVSCHEDULE" :args [(n 1) rates]}
             {:fname "FVSCHEDULE" :args [(n 10000) rates]}
             {:fname "FVSCHEDULE" :args [(n 1) (area [[0]])]}])))
