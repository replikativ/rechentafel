(ns rechentafel.fn.logical-test
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.math]
            [rechentafel.fn.logical]))

(defn- n [x] (v/number x))

(deftest constants
  (is (= v/TRUE  (f/call "TRUE" [])))
  (is (= v/FALSE (f/call "FALSE" []))))

(deftest not-and-or-xor
  (is (= v/FALSE (f/call "NOT" [v/TRUE])))
  (is (= v/TRUE  (f/call "NOT" [v/FALSE])))
  (is (= v/TRUE  (f/call "AND" [v/TRUE v/TRUE])))
  (is (= v/FALSE (f/call "AND" [v/TRUE v/FALSE])))
  (is (= v/TRUE  (f/call "OR"  [v/FALSE v/TRUE])))
  (is (= v/FALSE (f/call "OR"  [v/FALSE v/FALSE])))
  (is (= v/TRUE  (f/call "XOR" [v/TRUE v/FALSE])))
  (is (= v/FALSE (f/call "XOR" [v/TRUE v/TRUE]))))

(deftest and-or-numeric-coercion
  (is (= v/TRUE  (f/call "AND" [(n 1) (n 2)])))
  (is (= v/FALSE (f/call "AND" [(n 1) (n 0)])))
  (is (= v/FALSE (f/call "OR"  [(n 0) (n 0)])))
  (is (= v/ERR-VALUE (f/call "AND" [(v/string "x") v/BLANK]))))

(deftest choose
  (is (= (v/string "b") (f/call "CHOOSE" [(n 2) (v/string "a") (v/string "b") (v/string "c")])))
  (is (= v/ERR-VALUE (f/call "CHOOSE" [(n 4) (v/string "a") (v/string "b")]))))

(deftest if-lazy
  (is (= (n 1) (f/call-lazy {} "IF" [v/TRUE (n 1) (n 2)])))
  (is (= (n 2) (f/call-lazy {} "IF" [v/FALSE (n 1) (n 2)])))
  ;; 2-arg form: false path defaults to FALSE
  (is (= v/FALSE (f/call-lazy {} "IF" [v/FALSE (n 1)])))
  (is (= (n 1)   (f/call-lazy {} "IF" [v/TRUE (n 1)])))
  (is (= v/ERR-VALUE (f/call-lazy {} "IF" [v/ERR-VALUE (n 1) (n 2)]))))

(deftest iferror-ifna
  (is (= (n 0) (f/call-lazy {} "IFERROR" [v/ERR-NA (n 0)])))
  (is (= (n 5) (f/call-lazy {} "IFERROR" [(n 5) (n 0)])))
  (is (= (n 0) (f/call-lazy {} "IFNA" [v/ERR-NA (n 0)])))
  (is (= v/ERR-VALUE (f/call-lazy {} "IFNA" [v/ERR-VALUE (n 0)]))))

(deftest ifs-switch
  (is (= (v/string "y")
         (f/call-lazy {} "IFS" [v/FALSE (v/string "x") v/TRUE (v/string "y")])))
  (is (= v/ERR-NA
         (f/call-lazy {} "IFS" [v/FALSE (v/string "x") v/FALSE (v/string "y")])))
  (is (= (v/string "two")
         (f/call-lazy {} "SWITCH"
                      [(n 2) (n 1) (v/string "one") (n 2) (v/string "two") (v/string "def")])))
  (is (= (v/string "def")
         (f/call-lazy {} "SWITCH"
                      [(n 9) (n 1) (v/string "one") (n 2) (v/string "two") (v/string "def")])))
  (is (= v/ERR-NA
         (f/call-lazy {} "SWITCH"
                      [(n 9) (n 1) (v/string "one") (n 2) (v/string "two")]))))
