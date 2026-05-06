(ns rechentafel.fn.datetime-test
  "Datetime fns run on both runtimes via cljc.java-time (java.time on
  JVM, js-joda on cljs). Tests verify identical Excel semantics across
  both — DATE/YEAR roundtrip, ISOWEEKNUM, NETWORKDAYS, DATEDIF."
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.datetime]))

(defn- n [x] (v/number x))

(deftest date-constructor-roundtrip
  (let [serial (:v (f/call "DATE" [(n 2025) (n 4) (n 17)]))]
    (is (= 45764.0 serial))
    (is (= (n 2025) (f/call "YEAR" [(n serial)])))
    (is (= (n 4)    (f/call "MONTH" [(n serial)])))
    (is (= (n 17)   (f/call "DAY" [(n serial)])))
    ;; April 17 2025 is a Thursday (Sun=1 mapping → 5)
    (is (= (n 5)    (f/call "WEEKDAY" [(n serial)])))
    (is (= (n 4)    (f/call "WEEKDAY" [(n serial) (n 2)]))))

  ;; month overflow rolls forward
  (is (= 45779.0 (:v (f/call "DATE" [(n 2025) (n 4) (n 32)])))))

(deftest time-and-parts
  (let [t (:v (f/call "TIME" [(n 12) (n 30) (n 45)]))]
    (is (< (Math/abs (- t (/ (+ (* 12 3600) (* 30 60) 45) 86400.0))) 1e-9)))
  (is (= (n 12) (f/call "HOUR" [(n 0.5)])))
  (is (= (n 0)  (f/call "MINUTE" [(n 0.5)])))
  (is (= (n 0)  (f/call "SECOND" [(n 0.5)]))))

(deftest edate-eomonth
  ;; 45764 = 2025-04-17; +1 month = 2025-05-17 = 45794
  (is (= (n 45794) (f/call "EDATE" [(n 45764) (n 1)])))
  ;; EOMONTH(2025-04-17, 0) = 2025-04-30
  (is (= (n 45777) (f/call "EOMONTH" [(n 45764) (n 0)]))))

(deftest days-and-datedif
  (is (= (n 764) (f/call "DAYS" [(n 45764) (n 45000)])))
  (is (= (n 97)  (f/call "DATEDIF" [(n 10000) (n 45764) (v/string "Y")]))))

(deftest datevalue-parse
  (is (= (n 45764) (f/call "DATEVALUE" [(v/string "2025-04-17")])))
  (is (= v/ERR-VALUE (f/call "DATEVALUE" [(v/string "not-a-date")]))))

(deftest networkdays
  ;; Apr 11 Fri → Apr 17 Thu, 2025 = 5 business days (Sat/Sun excluded)
  (is (= (n 5) (f/call "NETWORKDAYS" [(n 45758) (n 45764)])))
  ;; Same span with a holiday on Mon Apr 14 → 4 business days
  (is (= (n 4) (f/call "NETWORKDAYS" [(n 45758) (n 45764) (n 45761)]))))

(deftest isoweeknum
  ;; ISO-8601 week numbering: weeks start Mon, week 1 contains the first
  ;; Thursday of the year. Dates constructed via DATE() so we don't hardcode
  ;; the 1900-leap-year serial offset.
  (let [d (fn [y m d] (:v (f/call "DATE" [(n y) (n m) (n d)])))]
    ;; 2023-01-01 is Sunday → tail of 2022's last ISO week (52)
    (is (= (n 52) (f/call "ISOWEEKNUM" [(n (d 2023 1 1))])))
    ;; 2023-01-02 is Monday → week 1 of 2023
    (is (= (n 1)  (f/call "ISOWEEKNUM" [(n (d 2023 1 2))])))
    ;; 2020-12-31 is Thursday → 2020 has 53 ISO weeks
    (is (= (n 53) (f/call "ISOWEEKNUM" [(n (d 2020 12 31))])))
    ;; 2025-04-17 is Thu of week 16
    (is (= (n 16) (f/call "ISOWEEKNUM" [(n (d 2025 4 17))])))))
