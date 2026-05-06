(ns rechentafel.oracle.fn.text-poi-test
  "Cross-check text functions against POI."
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

(deftest case-conversion
  (check! [{:fname "UPPER"  :args [(s "hello world")]}
           {:fname "LOWER"  :args [(s "HELLO")]}
           {:fname "PROPER" :args [(s "this is a test")]}
           {:fname "PROPER" :args [(s "o'neil's pub")]}]))

(deftest length-and-codes
  ;; POI's CODE returns a StringEval (bug); Excel/we return a number. Skip
  ;; CODE from cross-check and keep it in text-test.
  (check! [{:fname "LEN"  :args [(s "hello")]}
           {:fname "LEN"  :args [(s "")]}
           {:fname "CHAR" :args [(n 65)]}
           {:fname "CHAR" :args [(n 126)]}]))

(deftest clean-trim
  (check! [{:fname "TRIM"  :args [(s "   hello   world   ")]}
           {:fname "TRIM"  :args [(s "  a b  c  ")]}
           {:fname "CLEAN" :args [(s (str "ab" (char 9) "cd" (char 1) "ef"))]}
           {:fname "CLEAN" :args [(s "plain")]}]))

(deftest slice
  (check! [{:fname "LEFT"  :args [(s "hello") (n 3)]}
           {:fname "LEFT"  :args [(s "hello")]}
           {:fname "LEFT"  :args [(s "hello") (n 100)]}
           {:fname "LEFT"  :args [(s "hello") (n 0)]}
           {:fname "RIGHT" :args [(s "hello") (n 3)]}
           {:fname "RIGHT" :args [(s "hello")]}
           {:fname "RIGHT" :args [(s "hello") (n 100)]}
           {:fname "MID"   :args [(s "hello world") (n 7) (n 5)]}
           {:fname "MID"   :args [(s "abc") (n 2) (n 100)]}
           {:fname "MID"   :args [(s "abc") (n 10) (n 3)]}]))

(deftest find-search
  ;; POI's SEARCH is a plain case-insensitive indexOf (no wildcards). Our
  ;; impl supports * and ? per Excel; wildcard cases live in text-test.
  (check! [{:fname "FIND"   :args [(s "ll") (s "hello")]}
           {:fname "FIND"   :args [(s "o") (s "hello world") (n 6)]}
           {:fname "SEARCH" :args [(s "LL") (s "hello")]}
           {:fname "SEARCH" :args [(s "ell") (s "HELLO")]}]))

(deftest substitute-replace-rept
  (check! [{:fname "SUBSTITUTE" :args [(s "a a a") (s "a") (s "b")]}
           {:fname "SUBSTITUTE" :args [(s "a a a") (s "a") (s "b") (n 2)]}
           {:fname "REPLACE"    :args [(s "hello") (n 2) (n 3) (s "XX")]}
           {:fname "REPLACE"    :args [(s "abcdef") (n 1) (n 2) (s "ZZ")]}
           {:fname "REPT"       :args [(s "ab") (n 3)]}
           {:fname "REPT"       :args [(s "ab") (n 0)]}
           {:fname "EXACT"      :args [(s "abc") (s "abc")]}
           {:fname "EXACT"      :args [(s "abc") (s "ABC")]}]))

(deftest concat-textjoin
  (check! [{:fname "CONCATENATE" :args [(s "a") (s "b") (n 1)]}
           {:fname "CONCATENATE" :args [(s "x") (s "y") (s "z")]}
           {:fname "CONCAT"      :args [(s "a") (s "b") (s "c")]}
           {:fname "TEXTJOIN"    :args [(s ",") v/TRUE (s "a") (s "") (s "c")]}
           {:fname "TEXTJOIN"    :args [(s ",") v/FALSE (s "a") (s "") (s "c")]}]))

(deftest t-and-n
  (check! [{:fname "T"     :args [(s "x")]}
           {:fname "T"     :args [(n 5)]}
           {:fname "T"     :args [v/TRUE]}
           {:fname "N"     :args [(n 5)]}
           {:fname "N"     :args [(s "x")]}
           {:fname "N"     :args [v/TRUE]}
           {:fname "VALUE" :args [(s "3.14")]}
           {:fname "VALUE" :args [(s "1,234.56")]}]))

(deftest formatting
  (check! [{:fname "FIXED"       :args [(n 12345.678)]}
           {:fname "FIXED"       :args [(n 12345.678) (n 2) v/TRUE]}
           {:fname "FIXED"       :args [(n 12345.678) (n 0)]}
           {:fname "FIXED"       :args [(n -1234.5)]}
           {:fname "DOLLAR"      :args [(n 1234.5)]}
           {:fname "DOLLAR"      :args [(n -1234.5)]}
           {:fname "DOLLAR"      :args [(n 1234.5) (n 0)]}
           {:fname "NUMBERVALUE" :args [(s "3,14") (s ",")]}
           {:fname "NUMBERVALUE" :args [(s "1.234.567,89") (s ",") (s ".")]}
           {:fname "TEXT"        :args [(n 0.25) (s "0%")]}
           {:fname "TEXT"        :args [(s "hello") (s "@")]}]))
