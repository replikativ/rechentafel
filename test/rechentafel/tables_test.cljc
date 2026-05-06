(ns rechentafel.tables-test
  "Tests for structured (table) references.

  Covers the resolution rules from `.internal/lang-design.md` §3:
  default = data band only; #All / #Headers / #Data / #Totals /
  #This Row areas; column and column-range specifiers; combination
  forms like [#Headers],[col]."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- mk
  "Build a workbook with a table named TABLE seeded at A1, plus the
  given formula cells. Table layout:

      A     B
   1  Date  Amount   ← headers
   2  d1    10
   3  d2    20
   4  d3    30
   5  Tot   60       ← totals row (added when totals?=true)
  "
  [{:keys [totals? formulas]}]
  (-> (e/empty-workbook ["Sheet1"])
      ;; literal data
      (e/set-cell (c/pack 0 0 0) "Date")
      (e/set-cell (c/pack 0 0 1) "Amount")
      (e/set-cell (c/pack 0 1 0) "d1") (e/set-cell (c/pack 0 1 1) 10)
      (e/set-cell (c/pack 0 2 0) "d2") (e/set-cell (c/pack 0 2 1) 20)
      (e/set-cell (c/pack 0 3 0) "d3") (e/set-cell (c/pack 0 3 1) 30)
      (cond-> totals?
        (-> (e/set-cell (c/pack 0 4 0) "Tot")
            (e/set-cell (c/pack 0 4 1) 60)))
      (e/define-table "Sales"
        {:sheet 0
         :ref [0 0 (if totals? 4 3) 1]
         :columns ["Date" "Amount"]
         :header-rows 1
         :totals-rows (if totals? 1 0)})
      (as-> wb
            (reduce (fn [wb [r col input]]
                      (e/set-cell wb (c/pack 0 r col) input))
                    wb formulas))
      e/recalc))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))

;; ---------------------------------------------------------------------------
;; Default (data band) resolution

(deftest default-band-is-data
  ;; Sales[Amount] → B2:B4 (no headers, no totals)
  (let [wb (mk {:formulas [[6 0 "=SUM(Sales[Amount])"]]})]
    (is (= 60.0 (:v (at wb 6 0))))))

(deftest with-totals-default-still-data-only
  ;; default excludes totals row even when present
  (let [wb (mk {:totals? true :formulas [[6 0 "=SUM(Sales[Amount])"]]})]
    (is (= 60.0 (:v (at wb 6 0))))))

;; ---------------------------------------------------------------------------
;; Area keywords

(deftest all-area-includes-headers-and-totals
  (let [wb (mk {:totals? true
                :formulas [[6 0 "=COUNTA(Sales[#All])"]]})]
    ;; 5 rows × 2 cols = 10 cells, all populated
    (is (= 10.0 (:v (at wb 6 0))))))

(deftest headers-area
  (let [wb (mk {:formulas [[6 0 "=Sales[[#Headers],[Amount]]"]]})]
    (is (= "Amount" (:v (at wb 6 0))))))

(deftest totals-area
  (let [wb (mk {:totals? true
                :formulas [[6 0 "=Sales[[#Totals],[Amount]]"]]})]
    (is (= 60.0 (:v (at wb 6 0))))))

(deftest data-area-explicit
  (let [wb (mk {:totals? true
                :formulas [[6 0 "=SUM(Sales[[#Data],[Amount]])"]]})]
    (is (= 60.0 (:v (at wb 6 0))))))

;; ---------------------------------------------------------------------------
;; Column-range specifiers

(deftest column-range
  (let [wb (mk {:formulas [[6 0 "=COUNTA(Sales[[Date]:[Amount]])"]]})]
    ;; 3 data rows × 2 cols = 6 cells
    (is (= 6.0 (:v (at wb 6 0))))))

;; ---------------------------------------------------------------------------
;; This-row / @-shorthand

(deftest this-row-inside-data-band
  ;; Cell at row 2 (data row) computing Sales[@Amount] should equal B3 (=20).
  (let [wb (mk {:formulas [[2 5 "=Sales[@Amount]"]]})]
    (is (= 20.0 (:v (at wb 2 5))))))

(deftest this-row-outside-data-band-errors
  ;; Outside the data rows → no row to bind.
  (let [wb (mk {:formulas [[10 0 "=Sales[@Amount]"]]})]
    (is (= :ref (:v (at wb 10 0))))))

;; ---------------------------------------------------------------------------
;; Unknown table / column

(deftest unknown-table
  (let [wb (mk {:formulas [[6 0 "=SUM(Bogus[col])"]]})]
    (is (= :ref (:v (at wb 6 0))))))

(deftest unknown-column
  (let [wb (mk {:formulas [[6 0 "=SUM(Sales[Bogus])"]]})]
    (is (= :ref (:v (at wb 6 0))))))

;; ---------------------------------------------------------------------------
;; Round-trip through unparser

(deftest formulatext-roundtrip
  (let [wb (mk {:formulas [[6 0 "=SUM(Sales[Amount])"]
                           [6 1 "=FORMULATEXT(A7)"]]})]
    (is (= "=SUM(Sales[Amount])" (:v (at wb 6 1))))))

(deftest formulatext-roundtrip-areas
  (let [wb (mk {:formulas [[6 0 "=Sales[[#Headers],[Amount]]"]
                           [6 1 "=FORMULATEXT(A7)"]]})]
    (is (= "=Sales[[#Headers],[Amount]]" (:v (at wb 6 1))))))

;; ---------------------------------------------------------------------------
;; Dep tracking — mutating a cell inside the table should re-evaluate
;; downstream formulas referencing the table.

(deftest dep-graph-tracks-table-cells
  (let [wb (mk {:formulas [[6 0 "=SUM(Sales[Amount])"]]})]
    (is (= 60.0 (:v (at wb 6 0))))
    ;; bump Amount in row 2 from 20 to 200
    (let [wb (-> wb
                 (e/set-cell (c/pack 0 2 1) 200)
                 e/recalc)]
      (is (= 240.0 (:v (at wb 6 0)))))))
