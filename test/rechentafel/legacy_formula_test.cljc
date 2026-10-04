(ns rechentafel.legacy-formula-test
  "A formula a file does not mark as a dynamic array is a legacy formula:
   its value is a scalar, as in the Excel that wrote it. An array result is
   reduced by implicit intersection with the formula's row or column and
   never spills over the cells below (SpreadsheetBench 51262: a SUMIFS with
   a range of criteria spilled over the criteria list it read)."
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- v [wb r col] (:v (e/get-cell wb (c/pack 0 r col))))

(defn- wb-with [set-formula]
  ;; A1:A4 = 1..4; B1 = =A1:A4*10; B3 holds text, set after the formula
  ;; (the order a file is read in, row by row)
  (-> (reduce (fn [wb r] (e/set-cell wb (c/pack 0 r 0) (inc r))) (e/empty-workbook) (range 4))
      (set-formula (c/pack 0 0 1) "=A1:A4*10")
      (e/set-cell (c/pack 0 2 1) "kept")
      e/recalc))

(deftest a-legacy-formula-never-spills-over-the-cells-below
  (let [wb (wb-with e/set-legacy-formula)]
    (is (= 10.0 (double (v wb 0 1))) "the value in the formula's own row")
    (is (= "kept" (v wb 2 1)) "the cell below keeps its value")
    (is (empty? (:spills wb)))))

(deftest a-dynamic-formula-still-spills
  ;; set-cell is a formula typed today: it spills, and reports #SPILL! when
  ;; a cell it would cover holds a value
  (let [wb (-> (reduce (fn [wb r] (e/set-cell wb (c/pack 0 r 0) (inc r))) (e/empty-workbook) (range 4))
               (e/set-cell (c/pack 0 0 1) "=A1:A4*10")
               e/recalc)]
    (is (= [10.0 20.0 30.0 40.0] (mapv #(double (v wb % 1)) (range 4))))))

(deftest writing-a-legacy-cell-again-makes-it-a-current-formula
  (let [wb (-> (wb-with e/set-legacy-formula)
               (e/set-cell (c/pack 0 2 1) nil)
               (e/set-cell (c/pack 0 0 1) "=A1:A4*10")
               e/recalc)]
    (is (= [10.0 20.0 30.0 40.0] (mapv #(double (v wb % 1)) (range 4))))))
