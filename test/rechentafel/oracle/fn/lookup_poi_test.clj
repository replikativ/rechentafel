(ns rechentafel.oracle.fn.lookup-poi-test
  "Cross-check lookup functions against POI."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- area
  "Build an area from a 2D vector; accepts mixed scalars by wrapping them
  through the appropriate constructor."
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

(deftest match-modes
  (check! [{:fname "MATCH" :args [(n 25) (area [[10 20 30 40 50]]) (n 1)]}    ;; largest <=
           {:fname "MATCH" :args [(n 30) (area [[10 20 30 40 50]]) (n 0)]}    ;; exact
           {:fname "MATCH" :args [(n 25) (area [[50 40 30 20 10]]) (n -1)]}   ;; smallest >=
           {:fname "MATCH" :args [(s "b") (area [["a" "b" "c"]]) (n 0)]}]))

(deftest vlookup
  (let [table (area [[1 "apple"]
                     [2 "banana"]
                     [3 "cherry"]
                     [4 "date"]])]
    (check! [{:fname "VLOOKUP" :args [(n 2) table (n 2) v/FALSE]}
             {:fname "VLOOKUP" :args [(n 3) table (n 2) v/FALSE]}
             {:fname "VLOOKUP" :args [(n 2.5) table (n 2) v/TRUE]}     ;; approx → banana
             {:fname "VLOOKUP" :args [(n 99) table (n 2) v/FALSE]}])))  ;; #N/A

(deftest hlookup
  (let [table (area [[1 2 3 4]
                     ["a" "b" "c" "d"]])]
    (check! [{:fname "HLOOKUP" :args [(n 3) table (n 2) v/FALSE]}
             {:fname "HLOOKUP" :args [(n 1) table (n 2) v/FALSE]}
             {:fname "HLOOKUP" :args [(n 2.5) table (n 2) v/TRUE]}])))

(deftest index-fn
  (let [data (area [[1 2 3] [4 5 6] [7 8 9]])]
    (check! [{:fname "INDEX" :args [data (n 2) (n 3)]}
             {:fname "INDEX" :args [data (n 1) (n 1)]}
             {:fname "INDEX" :args [data (n 3) (n 2)]}])))

(deftest choose-lookup
  (check! [{:fname "CHOOSE" :args [(n 1) (s "a") (s "b") (s "c")]}
           {:fname "CHOOSE" :args [(n 3) (s "a") (s "b") (s "c")]}
           {:fname "LOOKUP" :args [(n 2) (area [[1 2 3 4]]) (area [["a" "b" "c" "d"]])]}
           {:fname "LOOKUP" :args [(n 2.5) (area [[1 2 3 4]]) (area [["a" "b" "c" "d"]])]}]))
