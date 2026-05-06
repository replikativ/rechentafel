(ns rechentafel.oracle.fn.logical-poi-test
  "Cross-check logical functions against POI."
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

(deftest and-or-not-xor
  (check! [{:fname "AND"  :args [v/TRUE v/TRUE v/TRUE]}
           {:fname "AND"  :args [v/TRUE v/FALSE]}
           {:fname "AND"  :args [(n 1) (n 2) (n 3)]}
           {:fname "OR"   :args [v/FALSE v/FALSE v/TRUE]}
           {:fname "OR"   :args [v/FALSE v/FALSE]}
           {:fname "OR"   :args [(n 0) (n 0) (n 5)]}
           {:fname "NOT"  :args [v/TRUE]}
           {:fname "NOT"  :args [v/FALSE]}
           {:fname "NOT"  :args [(n 0)]}
           {:fname "XOR"  :args [v/TRUE v/FALSE]}
           {:fname "XOR"  :args [v/TRUE v/TRUE v/FALSE]}]))

(deftest if-and-iferror
  (check! [{:fname "IF"      :args [v/TRUE (n 1) (n 2)]}
           {:fname "IF"      :args [v/FALSE (n 1) (n 2)]}
           {:fname "IF"      :args [(n 0) (n 1) (n 2)]}
           {:fname "IF"      :args [(n 5) (n 1) (n 2)]}
           {:fname "IFERROR" :args [(n 42) (s "fallback")]}
           {:fname "IFERROR" :args [v/ERR-DIV0 (n 99)]}
           {:fname "IFERROR" :args [v/ERR-NA (s "oops")]}
           {:fname "IFNA"    :args [v/ERR-NA (n 0)]}
           {:fname "IFNA"    :args [(n 5) (n 0)]}
           {:fname "IFNA"    :args [v/ERR-DIV0 (n 0)]}]))

(deftest true-false
  (check! [{:fname "TRUE"  :args []}
           {:fname "FALSE" :args []}]))

(deftest switch-ifs
  (check! [{:fname "IFS"    :args [v/FALSE (n 1) v/TRUE (n 2) v/FALSE (n 3)]}
           {:fname "SWITCH" :args [(n 2) (n 1) (s "one") (n 2) (s "two") (s "other")]}
           {:fname "SWITCH" :args [(n 99) (n 1) (s "one") (n 2) (s "two") (s "other")]}]))
