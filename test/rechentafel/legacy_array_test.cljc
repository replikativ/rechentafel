(ns rechentafel.legacy-array-test
  "Legacy (Ctrl+Shift+Enter) array formulas: the file fixes the range and
   the anchor fills exactly it, as Excel does — a single value in every
   cell, a one-row/one-column result repeated, #N/A outside the result, a
   1x1 range showing the top-left value without spilling — and a formula
   reading a cell of the range sees the anchor's value."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))
(defn- v [wb r col] (:v (at wb r col)))

(defn- array-wb
  "A workbook with literal `cells` [[r c input] …] and legacy array
   formulas `arrays` [[r0 c0 formula r1 c1] …], recalculated."
  [cells arrays]
  (e/recalc
   (reduce (fn [wb [r0 c0 f r1 c1]] (e/set-array-formula wb (c/pack 0 r0 c0) f r1 c1))
           (reduce (fn [wb [r col input]] (e/set-cell wb (c/pack 0 r col) input))
                   (e/empty-workbook) cells)
           arrays)))

(deftest a-single-value-fills-the-range
  (let [wb (array-wb [] [[0 0 "=1+1" 1 1]])]
    (is (= [2.0 2.0 2.0 2.0] (mapv double [(v wb 0 0) (v wb 0 1) (v wb 1 0) (v wb 1 1)])))))

(deftest a-result-fills-by-position-and-repeats-a-single-row-or-column
  (testing "a column into a taller range: #N/A below the result"
    (let [wb (array-wb [] [[0 0 "={1;2;3}" 3 0]])]
      (is (= [1.0 2.0 3.0] (mapv #(double (v wb % 0)) [0 1 2])))
      (is (= :na (v wb 3 0)))))
  (testing "a row repeated down"
    (let [wb (array-wb [] [[0 0 "={1,2}" 1 1]])]
      (is (= [1.0 2.0 1.0 2.0] (mapv double [(v wb 0 0) (v wb 0 1) (v wb 1 0) (v wb 1 1)])))))
  (testing "element-wise over a range"
    (let [wb (array-wb [[0 0 1] [1 0 2] [2 0 3]] [[0 1 "=A1:A3*10" 2 1]])]
      (is (= [10.0 20.0 30.0] (mapv #(double (v wb % 1)) [0 1 2]))))))

(deftest a-one-cell-array-formula-shows-its-first-value-and-does-not-spill
  (let [wb (array-wb [] [[0 0 "={5,6}" 0 0]])]
    (is (= 5.0 (double (v wb 0 0))))
    (is (nil? (v wb 0 1)) "no spill into B1")))

(deftest reading-a-cell-of-the-range-reads-the-anchor
  ;; C1 reads A3, which the anchor A1 writes: evaluated after it
  (let [wb (e/recalc (-> (e/empty-workbook)
                         (e/set-cell (c/pack 0 0 2) "=A3*10")
                         (e/set-array-formula (c/pack 0 0 0) "={1;2;3}" 2 0)))]
    (is (= 30.0 (double (v wb 0 2)))))
  (testing "and changes with it"
    (let [wb (e/recalc (-> (e/empty-workbook)
                           (e/set-cell (c/pack 0 0 3) 4)
                           (e/set-cell (c/pack 0 0 2) "=A3*10")
                           (e/set-array-formula (c/pack 0 0 0) "=D1*{1;2;3}" 2 0)))
          wb (e/recalc (e/set-cell wb (c/pack 0 0 3) 5))]
      (is (= 150.0 (double (v wb 0 2)))))))

(defn- formula-value [cells f]
  (let [wb (e/recalc (e/set-cell (reduce (fn [wb [r col x]] (e/set-cell wb (c/pack 0 r col) x)) (e/empty-workbook) cells)
                                 (c/pack 0 9 9) f))]
    (:v (e/get-cell wb (c/pack 0 9 9)))))

(deftest array-idioms-from-real-workbooks
  (testing "FREQUENCY keeps its counts in the bins' given order"
    ;; the nearest value to 7 among 5, 8, 12 (bins |5-7|=2, |8-7|=1, |12-7|=5)
    (is (= 8.0 (double (formula-value [[0 0 5] [0 1 8] [0 2 12]] "=LOOKUP(1,1/FREQUENCY(0,ABS(A1:C1-7)),A1:C1)")))))
  (testing "COUNTIF with an array of criteria counts each"
    (is (= 3.0 (double (formula-value [[0 0 "a"] [1 0 "b"] [2 0 "a"]] "=SUM(COUNTIF(A1:A3,{\"a\",\"b\"}))"))))
    (is (= 2.0 (double (formula-value [[0 0 "a"] [1 0 "b"] [2 0 "a"]] "=MATCH(1,INDEX(COUNTIF(A1:A3,A1:A3),0,0),0)")))))
  (testing "an omitted SEQUENCE argument is its default"
    (is (= 6.0 (double (formula-value [] "=SUM(SEQUENCE(,3))"))))))
