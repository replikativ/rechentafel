(ns rechentafel.threed-test
  "Tests for 3D references — Sheet1:Sheet3!A1:B5 form. Excel restricts
  these to ~18 aggregate functions; non-aggregates either auto-collapse
  to the current sheet's slab (when in range) or return #VALUE!."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- mk-3sheets
  "Three sheets, each with a small grid:
     Sheet0/A1=1  B1=10
     Sheet1/A1=2  B1=20
     Sheet2/A1=3  B1=30"
  []
  (-> (e/empty-workbook ["S1" "S2" "S3" "S4"])
      (e/set-cell (c/pack 0 0 0) 1) (e/set-cell (c/pack 0 0 1) 10)
      (e/set-cell (c/pack 1 0 0) 2) (e/set-cell (c/pack 1 0 1) 20)
      (e/set-cell (c/pack 2 0 0) 3) (e/set-cell (c/pack 2 0 1) 30)
      (e/set-cell (c/pack 3 0 0) 999)))   ;; S4 — outside the range, should never be touched

(defn- with-formulas [wb & cells]
  (e/recalc
   (reduce (fn [wb [r col input]]
             (e/set-cell wb (c/pack 0 r col) input))
           wb cells)))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))

;; ---------------------------------------------------------------------------
;; Single-cell 3D ref through aggregates

(deftest sum-across-sheets-single-cell
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S1:S3!A1)"]))]
    (is (= 6.0 (:v (at wb 4 0))))))

(deftest average-across-sheets
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=AVERAGE(S1:S3!B1)"]))]
    (is (= 20.0 (:v (at wb 4 0))))))

(deftest count-across-sheets
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=COUNT(S1:S3!A1:B1)"]))]
    (is (= 6.0 (:v (at wb 4 0))))))

(deftest min-across-sheets
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=MIN(S1:S3!A1:B1)"]))]
    (is (= 1.0 (:v (at wb 4 0))))))

(deftest max-across-sheets
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=MAX(S1:S3!A1:B1)"]))]
    (is (= 30.0 (:v (at wb 4 0))))))

(deftest product-across-sheets
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=PRODUCT(S1:S3!A1)"]))]
    ;; 1 * 2 * 3 = 6
    (is (= 6.0 (:v (at wb 4 0))))))

;; ---------------------------------------------------------------------------
;; 3D range over a multi-cell area

(deftest sum-3d-multi-cell
  ;; SUM of every cell in A1:B1 across S1, S2, S3 = 1+10+2+20+3+30 = 66
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S1:S3!A1:B1)"]))]
    (is (= 66.0 (:v (at wb 4 0))))))

;; ---------------------------------------------------------------------------
;; Out-of-range sheet not touched

(deftest s4-not-touched-by-3d-range
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S1:S3!A1)"]))]
    (is (= 6.0 (:v (at wb 4 0))))      ;; S4's 999 not included
    (is (= 999.0 (:v (e/get-cell wb (c/pack 3 0 0))))))) ;; S4 unchanged

;; ---------------------------------------------------------------------------
;; Reverse-order 3D ref normalises

(deftest reverse-order-3d-still-works
  ;; S3:S1 should equal S1:S3 in Excel (PutInOrder).
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S3:S1!A1)"]))]
    (is (= 6.0 (:v (at wb 4 0))))))

;; ---------------------------------------------------------------------------
;; Dep tracking — mutating a cell in any spanned sheet should re-eval

(deftest dep-graph-spans-3d-sheets
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S1:S3!A1)"]))]
    (is (= 6.0 (:v (at wb 4 0))))
    ;; bump S2!A1 from 2 to 200
    (let [wb (-> wb (e/set-cell (c/pack 1 0 0) 200) e/recalc)]
      (is (= 204.0 (:v (at wb 4 0)))))
    ;; bump S4!A1 — should NOT trigger a recalc (out of range)
    (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S1:S3!A1)"])
                 (e/set-cell (c/pack 3 0 0) 12345)
                 e/recalc)]
      (is (= 6.0 (:v (at wb 4 0)))))))

;; ---------------------------------------------------------------------------
;; Round-trip through unparser

(deftest formulatext-3d
  (let [wb (-> (mk-3sheets) (with-formulas [4 0 "=SUM(S1:S3!A1:B1)"]
                              [4 1 "=FORMULATEXT(A5)"]))]
    (is (= "=SUM(S1:S3!A1:B1)" (:v (at wb 4 1))))))
