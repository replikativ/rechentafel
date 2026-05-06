(ns rechentafel.oracle.fn.datetime-poi-test
  "Cross-check datetime functions against POI."
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

(deftest date-constructor
  (check! [{:fname "DATE"  :args [(n 2025) (n 4) (n 17)]}
           {:fname "DATE"  :args [(n 2025) (n 4) (n 32)]}  ;; rolls
           {:fname "DATE"  :args [(n 2000) (n 1) (n 1)]}
           {:fname "DATE"  :args [(n 2025) (n 13) (n 1)]}  ;; month overflow
           {:fname "YEAR"  :args [(n 45764)]}
           {:fname "MONTH" :args [(n 45764)]}
           {:fname "DAY"   :args [(n 45764)]}]))

(deftest weekday
  (check! [{:fname "WEEKDAY" :args [(n 45764)]}           ;; default Sun=1
           {:fname "WEEKDAY" :args [(n 45764) (n 2)]}     ;; Mon=1
           {:fname "WEEKDAY" :args [(n 45764) (n 3)]}]))  ;; Mon=0

(deftest time-and-parts
  (check! [{:fname "TIME"   :args [(n 12) (n 30) (n 45)]}
           {:fname "TIME"   :args [(n 23) (n 59) (n 59)]}
           {:fname "HOUR"   :args [(n 0.5)]}
           {:fname "HOUR"   :args [(n 45764.75)]}
           {:fname "MINUTE" :args [(n 0.5)]}
           {:fname "MINUTE" :args [(n 45764.875)]}
           {:fname "SECOND" :args [(n 0.5)]}]))

(deftest edate-eomonth
  (check! [{:fname "EDATE"   :args [(n 45764) (n 1)]}
           {:fname "EDATE"   :args [(n 45764) (n -3)]}
           {:fname "EOMONTH" :args [(n 45764) (n 0)]}
           {:fname "EOMONTH" :args [(n 45764) (n 2)]}]))

(deftest days-datedif
  (check! [{:fname "DAYS"    :args [(n 45764) (n 45000)]}
           {:fname "DATEDIF" :args [(n 10000) (n 45764) (s "Y")]}
           {:fname "DATEDIF" :args [(n 10000) (n 45764) (s "M")]}
           {:fname "DATEDIF" :args [(n 10000) (n 45764) (s "D")]}]))

(deftest datevalue
  (check! [{:fname "DATEVALUE" :args [(s "2025-04-17")]}
           {:fname "DATEVALUE" :args [(s "1/1/2000")]}]))

(deftest networkdays
  (check! [{:fname "NETWORKDAYS" :args [(n 45758) (n 45764)]}
           {:fname "NETWORKDAYS" :args [(n 45758) (n 45764) (n 45761)]}
           {:fname "WORKDAY"     :args [(n 45758) (n 10)]}]))

(deftest now-today
  ;; NOW and TODAY are volatile; we can't cross-check their absolute values
  ;; but we can verify they return numeric serials.
  (is (v/num? (f/call "TODAY" [])))
  (is (v/num? (f/call "NOW" []))))
