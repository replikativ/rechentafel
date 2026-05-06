(ns rechentafel.fn.info-test
  "Sanity tests for CELL, SHEET, SHEETS, and INFO. Exercises the
  evaluator's lazy-ctx so that these functions can read workbook
  metadata at eval time."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.value :as v]
            [rechentafel.functions.all]))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))

(defn- mk [sheets & cells]
  (e/recalc
   (reduce (fn [wb [sheet row col input]]
             (e/set-cell wb (c/pack sheet row col) input))
           (e/empty-workbook sheets)
           cells)))

;; ---------------------------------------------------------------------------
;; CELL

(deftest cell-row-col-address
  (let [wb (mk ["Sheet1"]
               [0 4 2 123]                         ;; C5
               [0 0 0 "=CELL(\"row\",C5)"]
               [0 0 1 "=CELL(\"col\",C5)"]
               [0 0 2 "=CELL(\"address\",C5)"])]
    (is (= {:t :num :v 5.0}   (at wb 0 0)))
    (is (= {:t :num :v 3.0}   (at wb 0 1)))
    (is (= {:t :str :v "$C$5"} (at wb 0 2)))))

(deftest cell-contents-and-type
  (let [wb (mk ["Sheet1"]
               [0 0 0 42]
               [0 0 1 "hello"]
               [0 1 0 "=CELL(\"contents\",A1)"]
               [0 1 1 "=CELL(\"contents\",B1)"]
               [0 2 0 "=CELL(\"type\",A1)"]
               [0 2 1 "=CELL(\"type\",B1)"]
               [0 2 2 "=CELL(\"type\",Z99)"])]   ;; blank
    (is (= {:t :num :v 42.0}    (at wb 1 0)))
    (is (= {:t :str :v "hello"} (at wb 1 1)))
    (is (= {:t :str :v "v"}     (at wb 2 0)))
    (is (= {:t :str :v "l"}     (at wb 2 1)))
    (is (= {:t :str :v "b"}     (at wb 2 2)))))

(deftest cell-sheetname
  (let [wb (mk ["Main" "Other"]
               [0 0 0 "=CELL(\"sheetname\",A1)"])]
    (is (= {:t :str :v "Main"} (at wb 0 0)))))

(deftest cell-no-ref-uses-current-cell
  (let [wb (mk ["Sheet1"]
               [0 3 5 "=CELL(\"row\")"]
               [0 3 6 "=CELL(\"col\")"])]
    (is (= {:t :num :v 4.0} (at wb 3 5)))
    (is (= {:t :num :v 7.0} (at wb 3 6)))))

;; ---------------------------------------------------------------------------
;; SHEET / SHEETS

(deftest sheet-no-arg-is-current-sheet
  (let [wb (mk ["Sheet1" "Sheet2"]
               [0 0 0 "=SHEET()"])]
    (is (= {:t :num :v 1.0} (at wb 0 0)))))

(deftest sheet-with-qualified-ref
  (let [wb (mk ["Sheet1" "Sheet2" "Sheet3"]
               [0 0 0 "=SHEET(Sheet2!A1)"]
               [0 0 1 "=SHEET(Sheet3!B2)"])]
    (is (= {:t :num :v 2.0} (at wb 0 0)))
    (is (= {:t :num :v 3.0} (at wb 0 1)))))

(deftest sheet-with-name-string
  (let [wb (mk ["Sheet1" "Sheet2"]
               [0 0 0 "=SHEET(\"Sheet2\")"])]
    (is (= {:t :num :v 2.0} (at wb 0 0)))))

(deftest sheets-count-no-arg
  (let [wb (mk ["A" "B" "C" "D"]
               [0 0 0 "=SHEETS()"])]
    (is (= {:t :num :v 4.0} (at wb 0 0)))))

;; ---------------------------------------------------------------------------
;; INFO

(deftest info-known-types
  (let [wb (mk ["Sheet1"]
               [0 0 0 "=INFO(\"numfile\")"]
               [0 0 1 "=INFO(\"recalc\")"]
               [0 0 2 "=INFO(\"directory\")"])]
    (is (= {:t :num :v 1.0}          (at wb 0 0)))
    (is (= {:t :str :v "Automatic"}  (at wb 0 1)))
    (is (= {:t :str :v ""}           (at wb 0 2)))))

(deftest info-unknown-type
  (let [wb (mk ["Sheet1"] [0 0 0 "=INFO(\"bogus\")"])]
    (is (= :value (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; ISFORMULA (Excel 2013+): TRUE iff the cell contains a formula.

(deftest isformula-detects-formula-cells
  (let [wb (mk ["Sheet1"]
               [0 0 0 "=1+1"]              ;; A1: formula
               [0 1 0 42]                  ;; A2: literal
               [0 2 0 "hello"]             ;; A3: literal string
               [0 0 1 "=ISFORMULA(A1)"]    ;; → TRUE
               [0 1 1 "=ISFORMULA(A2)"]    ;; → FALSE
               [0 2 1 "=ISFORMULA(A3)"]    ;; → FALSE
               [0 3 1 "=ISFORMULA(Z99)"])] ;; blank → FALSE
    (is (= {:t :bool :v true}  (at wb 0 1)))
    (is (= {:t :bool :v false} (at wb 1 1)))
    (is (= {:t :bool :v false} (at wb 2 1)))
    (is (= {:t :bool :v false} (at wb 3 1)))))
