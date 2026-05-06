(ns rechentafel.unparse-test
  "Tests for `rechentafel.unparse/unparse` and the FORMULATEXT function.

  The contract for the unparser is round-trip stability at the AST
  level: parse(unparse(parse(s))) ≡ parse(s). The source string itself
  may differ in case/whitespace — Excel normalises in the same way."
  (:require #?(:clj  [clojure.test :refer [deftest is testing are]]
               :cljs [cljs.test :refer-macros [deftest is testing are]])
            [rechentafel.parser :as parser]
            [rechentafel.unparse :as unparse]
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- u [src]   (-> src parser/parse unparse/unparse))
(defn- roundtrip [src]
  (let [ast1 (parser/parse src)
        s2   (unparse/unparse ast1)
        ast2 (parser/parse s2)]
    [ast1 ast2]))

;; ---------------------------------------------------------------------------
;; Atoms

(deftest numbers
  (is (= "0"      (u "0")))
  (is (= "42"     (u "42")))
  (is (= "3.14"   (u "3.14")))
  (is (= "1000"   (u "1000.0"))))

(deftest strings
  (is (= "\"hi\""           (u "\"hi\"")))
  (is (= "\"with \"\"q\"\"\"" (u "\"with \"\"q\"\"\""))))

(deftest booleans-and-errors
  (is (= "TRUE"   (u "TRUE")))
  (is (= "FALSE"  (u "false")))
  (is (= "#REF!"  (u "#REF!")))
  (is (= "#DIV/0!" (u "#DIV/0!")))
  (is (= "#N/A"   (u "#N/A"))))

;; ---------------------------------------------------------------------------
;; Refs and ranges

(deftest plain-refs
  (are [s] (= s (u s))
    "A1"
    "B2"
    "$A$1"
    "$A1"
    "A$1"
    "AA10"))

(deftest sheet-qualified
  (is (= "Sheet1!A1"        (u "Sheet1!A1")))
  (is (= "'My Sheet'!A1"    (u "'My Sheet'!A1")))
  (is (= "Sheet1!A1:B5"     (u "Sheet1!A1:B5"))))

(deftest three-d-refs
  (is (= "Sheet1:Sheet3!A1"     (u "Sheet1:Sheet3!A1")))
  (is (= "Sheet1:Sheet3!A1:B5"  (u "Sheet1:Sheet3!A1:B5"))))

(deftest whole-col-row
  (is (= "A:A"    (u "A:A")))
  (is (= "1:1"    (u "1:1")))
  (is (= "$A:$C"  (u "$A:$C"))))

;; ---------------------------------------------------------------------------
;; Operators with precedence

(deftest binops-flat
  (are [s] (= s (u s))
    "1+2"
    "1-2"
    "1*2"
    "1/2"
    "2^3"
    "1=2"
    "1<>2"
    "\"a\"&\"b\""))

(deftest binop-parens
  ;; canonical: parens only where precedence demands them
  (is (= "1+2*3"    (u "1+2*3")))
  (is (= "(1+2)*3"  (u "(1+2)*3")))
  (is (= "1+2+3"    (u "1+2+3")))           ;; left-assoc: no inner parens
  (is (= "1-(2-3)"  (u "1-(2-3)")))         ;; right side at same prec → parens
  (is (= "2^3^4"    (u "2^3^4"))))           ;; pow is right-assoc

(deftest unary
  (is (= "-A1"     (u "-A1")))
  (is (= "+A1"     (u "+A1")))
  (is (= "-(1+2)"  (u "-(1+2)"))))

(deftest postfix
  (is (= "A1%"     (u "A1%")))
  (is (= "(1+2)%"  (u "(1+2)%"))))

;; ---------------------------------------------------------------------------
;; Calls + arrays

(deftest calls
  (is (= "SUM(A1:A10)"        (u "sum(a1:a10)")))   ;; case canonicalised
  (is (= "IF(A1=0,1,2)"       (u "if(a1=0,1,2)")))
  (is (= "MAX(1,2,3)"         (u "MAX(1,2,3)"))))

(deftest array-literal
  (is (= "{1,2,3}"            (u "{1,2,3}")))
  (is (= "{1,2;3,4}"          (u "{1,2;3,4}"))))

;; ---------------------------------------------------------------------------
;; Round-trip property tests

(deftest round-trip-stable
  (testing "parse → unparse → parse yields the same AST"
    (doseq [s ["A1+B1"
               "SUM(A1:A10)+1"
               "IF(A1=0,B1,C1)"
               "(A1,B1,C1)"
               "Sheet1:Sheet3!A1:B5"
               "{1,2;3,4}"
               "A1:B2 B1:C2"     ;; intersection
               "-A1^2"           ;; unary vs pow
               "1+2*3-4/2^2"]]
      (let [[ast1 ast2] (roundtrip s)]
        (is (= ast1 ast2) (str "round-trip drift on: " s))))))

;; ---------------------------------------------------------------------------
;; FORMULATEXT integration

(defn- mk [& cells]
  (e/recalc
   (reduce (fn [wb [r col input]]
             (e/set-cell wb (c/pack 0 r col) input))
           (e/empty-workbook)
           cells)))

(deftest formulatext-basic
  (let [wb (mk [0 0 "=A2+B2"]
               [0 1 "=FORMULATEXT(A1)"])]
    (is (= {:t :str :v "=A2+B2"} (e/get-cell wb (c/pack 0 0 1))))))

(deftest formulatext-non-formula
  (let [wb (mk [0 0 42]
               [0 1 "=FORMULATEXT(A1)"])]
    (is (= :na (:v (e/get-cell wb (c/pack 0 0 1)))))))

(deftest formulatext-blank
  (let [wb (mk [0 1 "=FORMULATEXT(Z99)"])]
    (is (= :na (:v (e/get-cell wb (c/pack 0 0 1)))))))

(deftest formulatext-canonicalises
  ;; `sum(a1)` user-typed but FORMULATEXT returns canonical "=SUM(A1)"
  (let [wb (mk [0 0 "=sum(a1:a3)"]
               [0 1 "=FORMULATEXT(A1)"])]
    (is (= "=SUM(A1:A3)" (:v (e/get-cell wb (c/pack 0 0 1)))))))

(deftest formulatext-tracks-relative-refs
  ;; A1 = =D1+1, A2 = =D2+1. FORMULATEXT must report live coordinates,
  ;; not the rc-normalised template (which would render as RC[3] etc.).
  ;; D column is unrelated so we don't accidentally wire up a cycle.
  (let [wb (mk [0 0 "=D1+1"]
               [1 0 "=D2+1"]
               [0 1 "=FORMULATEXT(A1)"]
               [1 1 "=FORMULATEXT(A2)"])]
    (is (= "=D1+1" (:v (e/get-cell wb (c/pack 0 0 1)))))
    (is (= "=D2+1" (:v (e/get-cell wb (c/pack 0 1 1)))))))
