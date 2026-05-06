(ns rechentafel.fn.text-test
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.math]
            [rechentafel.fn.text]))

(defn- s [x] (v/string x))
(defn- n [x] (v/number x))

(deftest case-conversion
  (is (= (s "HELLO") (f/call "UPPER" [(s "hello")])))
  (is (= (s "hello") (f/call "LOWER" [(s "HELLO")])))
  (is (= (s "Hello World") (f/call "PROPER" [(s "hello world")])))
  (is (= (s "A'Neil") (f/call "PROPER" [(s "a'neil")]))))

(deftest length-and-codes
  (is (= (n 5) (f/call "LEN" [(s "hello")])))
  (is (= (s "A") (f/call "CHAR" [(n 65)])))
  (is (= (n 65) (f/call "CODE" [(s "A")])))
  (is (= (n 65) (f/call "CODE" [(s "Abc")])))
  (is (= v/ERR-VALUE (f/call "CHAR" [(n 0)])))
  (is (= v/ERR-VALUE (f/call "CODE" [(s "")]))))

(deftest trim-and-clean
  (is (= (s "hello world") (f/call "TRIM" [(s "   hello   world   ")])))
  (is (= (s "abcd") (f/call "CLEAN" [(s (str "ab" (char 9) "cd"))]))))

(deftest left-right-mid
  (is (= (s "hel") (f/call "LEFT" [(s "hello") (n 3)])))
  (is (= (s "h")   (f/call "LEFT" [(s "hello")])))
  (is (= (s "llo") (f/call "RIGHT" [(s "hello") (n 3)])))
  (is (= (s "world") (f/call "MID" [(s "hello world") (n 7) (n 5)])))
  (is (= (s "") (f/call "MID" [(s "hello") (n 100) (n 5)])))
  (is (= v/ERR-VALUE (f/call "MID" [(s "x") (n 0) (n 1)]))))

(deftest find-search
  (is (= (n 3) (f/call "FIND" [(s "ll") (s "hello")])))
  (is (= v/ERR-VALUE (f/call "FIND" [(s "LL") (s "hello")])))   ;; case-sensitive
  (is (= (n 3) (f/call "SEARCH" [(s "LL") (s "hello")])))        ;; case-insensitive
  (is (= (n 1) (f/call "SEARCH" [(s "h*o") (s "hello")])))
  (is (= (n 2) (f/call "SEARCH" [(s "?l") (s "hello")]))))

(deftest exact-substitute-replace-rept
  (is (= v/TRUE  (f/call "EXACT" [(s "abc") (s "abc")])))
  (is (= v/FALSE (f/call "EXACT" [(s "abc") (s "ABC")])))
  (is (= (s "b b b") (f/call "SUBSTITUTE" [(s "a a a") (s "a") (s "b")])))
  (is (= (s "a b a") (f/call "SUBSTITUTE" [(s "a a a") (s "a") (s "b") (n 2)])))
  (is (= (s "hXXo") (f/call "REPLACE" [(s "hello") (n 2) (n 3) (s "XX")])))
  (is (= (s "ababab") (f/call "REPT" [(s "ab") (n 3)])))
  (is (= (s "") (f/call "REPT" [(s "ab") (n 0)]))))

(deftest concat-and-textjoin
  (is (= (s "ab1") (f/call "CONCATENATE" [(s "a") (s "b") (n 1)])))
  (is (= (s "a,c") (f/call "TEXTJOIN" [(s ",") v/TRUE (s "a") (s "") (s "c")])))
  (is (= (s "a,,c") (f/call "TEXTJOIN" [(s ",") v/FALSE (s "a") (s "") (s "c")]))))

(deftest t-n-value
  (is (= (s "x")   (f/call "T" [(s "x")])))
  (is (= (s "")    (f/call "T" [(n 5)])))
  (is (= (n 5)     (f/call "N" [(n 5)])))
  (is (= (n 0)     (f/call "N" [(s "x")])))
  (is (= (n 1)     (f/call "N" [v/TRUE])))
  (is (= (n 3.14)  (f/call "VALUE" [(s "3.14")])))
  (is (= v/ERR-VALUE (f/call "VALUE" [(s "abc")]))))

;; ---------------------------------------------------------------------------
;; Unicode helpers and TEXTBEFORE/TEXTAFTER/TEXTSPLIT (Excel 2013+/365).
;; Vectors cross-checked against IronCalc src/functions/text.rs.

(deftest unicode-helpers
  (is (= (n 65) (f/call "UNICODE" [(s "A")])))
  (is (= (n 97) (f/call "UNICODE" [(s "abc")])))         ;; first char only
  (is (= (s "A") (f/call "UNICHAR" [(n 65)])))
  (is (= (s "€") (f/call "UNICHAR" [(n 8364)])))
  (testing "out-of-range or empty input"
    (is (= v/ERR-VALUE (f/call "UNICHAR" [(n 0)])))
    (is (= v/ERR-VALUE (f/call "UNICHAR" [(n -1)])))
    (is (= v/ERR-VALUE (f/call "UNICODE" [(s "")])))))

(deftest text-before-after-basic
  (is (= (s "hello") (f/call "TEXTBEFORE" [(s "hello,world") (s ",")])))
  (is (= (s "world") (f/call "TEXTAFTER"  [(s "hello,world") (s ",")])))
  (testing "not-found returns #N/A by default"
    (is (= :na (:v (f/call "TEXTBEFORE" [(s "hello") (s "x")]))))
    (is (= :na (:v (f/call "TEXTAFTER"  [(s "hello") (s "x")])))))
  (testing "explicit if-not-found arg overrides"
    (is (= (s "MISSING")
           (f/call "TEXTBEFORE"
                   [(s "hello") (s "x") (n 1) (n 0) (n 0) (s "MISSING")])))))

(deftest text-before-after-instance
  ;; Positive instance counts from start; negative counts from end.
  (is (= (s "a.b") (f/call "TEXTBEFORE" [(s "a.b.c.d") (s ".") (n 2)])))
  (is (= (s "c.d") (f/call "TEXTAFTER"  [(s "a.b.c.d") (s ".") (n 2)])))
  (testing "instance = -1 → last occurrence"
    (is (= (s "a.b.c") (f/call "TEXTBEFORE" [(s "a.b.c.d") (s ".") (n -1)])))
    (is (= (s "d")     (f/call "TEXTAFTER"  [(s "a.b.c.d") (s ".") (n -1)])))))

(deftest text-before-after-case-insensitive
  ;; match_mode = 1 → case-insensitive search.
  (is (= (s "H")   (f/call "TEXTBEFORE" [(s "HELLO") (s "e") (n 1) (n 1)])))
  (is (= (s "LLO") (f/call "TEXTAFTER"  [(s "HELLO") (s "e") (n 1) (n 1)]))))

(deftest text-split-scalar
  ;; Our engine lacks spill ranges, so TEXTSPLIT collapses to the first
  ;; fragment. Enough to prove the parse runs; full array semantics are a
  ;; separate feature.
  (is (= (s "a") (f/call "TEXTSPLIT" [(s "a,b,c") (s ",")])))
  (is (= (s "")  (f/call "TEXTSPLIT" [(s ",a,b") (s ",")])))
  (is (= (s "a") (f/call "TEXTSPLIT"
                         [(s ",a,b") (s ",") v/BLANK (n 1)]))))  ;; ignore-empty

(deftest number-formatting
  (is (= (n 3.14) (f/call "NUMBERVALUE" [(s "3,14") (s ",")])))
  (is (= (n 1234567.89)
         (f/call "NUMBERVALUE" [(s "1.234.567,89") (s ",") (s ".")])))
  (is (= (s "12,345.68") (f/call "FIXED" [(n 12345.678)])))
  (is (= (s "12345.68") (f/call "FIXED" [(n 12345.678) (n 2) v/TRUE])))
  (is (= (s "$1,234.50") (f/call "DOLLAR" [(n 1234.5)])))
  (is (= (s "($1,234.50)") (f/call "DOLLAR" [(n -1234.5)])))
  (is (= (s "25%") (f/call "TEXT" [(n 0.25) (s "0%")])))
  (is (= (s "hello") (f/call "TEXT" [(s "hello") (s "@")]))))
