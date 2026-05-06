(ns rechentafel.array-fn-test
  "M3 dynamic-array function set:
    - shape constructors: SEQUENCE, RANDARRAY, MUNIT, TRANSPOSE
    - reshape/select: HSTACK, VSTACK, CHOOSEROWS, CHOOSECOLS,
      DROP, TAKE, EXPAND, TOROW, TOCOL, WRAPROWS, WRAPCOLS
    - filter/sort: UNIQUE, SORT, SORTBY, FILTER
    - LAMBDA helpers: MAP, REDUCE, SCAN, BYROW, BYCOL, MAKEARRAY"
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- mk [& cells]
  (e/recalc
   (reduce (fn [wb [r col input]]
             (e/set-cell wb (c/pack 0 r col) input))
           (e/empty-workbook)
           cells)))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))
(defn- v  [wb r col] (:v (at wb r col)))

;; ---------------------------------------------------------------------------
;; Shape constructors

(deftest sequence-defaults
  (let [wb (mk [0 0 "=SEQUENCE(5)"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 5.0 (v wb 4 0)))))

(deftest sequence-with-cols
  (let [wb (mk [0 0 "=SEQUENCE(2,3)"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 3.0 (v wb 0 2)))
    (is (= 4.0 (v wb 1 0)))
    (is (= 6.0 (v wb 1 2)))))

(deftest sequence-with-start-step
  (let [wb (mk [0 0 "=SEQUENCE(4,1,10,5)"])]
    (is (= 10.0 (v wb 0 0)))
    (is (= 25.0 (v wb 3 0)))))

(deftest munit-identity
  (let [wb (mk [0 0 "=MUNIT(3)"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 0.0 (v wb 0 1)))
    (is (= 1.0 (v wb 1 1)))
    (is (= 1.0 (v wb 2 2)))))

(deftest transpose-spills
  (let [wb (mk [0 0 "=TRANSPOSE({1,2,3})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 2.0 (v wb 1 0)))
    (is (= 3.0 (v wb 2 0)))))

;; ---------------------------------------------------------------------------
;; Reshape / select

(deftest hstack-rows
  (let [wb (mk [0 0 "=HSTACK({1;2}, {3;4}, {5;6})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 3.0 (v wb 0 1)))
    (is (= 5.0 (v wb 0 2)))
    (is (= 6.0 (v wb 1 2)))))

(deftest vstack-rows
  (let [wb (mk [0 0 "=VSTACK({1,2}, {3,4})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 4.0 (v wb 1 1)))))

(deftest chooserows-positive-and-negative
  (let [wb (mk [0 0 "=CHOOSEROWS({10;20;30;40},1,3)"]
               [3 0 "=CHOOSEROWS({10;20;30;40},-1)"])]
    (is (= 10.0 (v wb 0 0)))
    (is (= 30.0 (v wb 1 0)))
    (is (= 40.0 (v wb 3 0)))))

(deftest choosecols-positive-and-negative
  (let [wb (mk [0 0 "=CHOOSECOLS({1,2,3,4},2,4)"])]
    (is (= 2.0 (v wb 0 0)))
    (is (= 4.0 (v wb 0 1)))))

(deftest drop-leading-rows
  (let [wb (mk [0 0 "=DROP({1;2;3;4}, 2)"])]
    (is (= 3.0 (v wb 0 0)))
    (is (= 4.0 (v wb 1 0)))))

(deftest take-leading
  (let [wb (mk [0 0 "=TAKE({1;2;3;4;5}, 2)"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 2.0 (v wb 1 0)))))

(deftest take-trailing
  (let [wb (mk [0 0 "=TAKE({1;2;3;4;5}, -2)"])]
    (is (= 4.0 (v wb 0 0)))
    (is (= 5.0 (v wb 1 0)))))

(deftest toRow-flatten
  (let [wb (mk [0 0 "=TOROW({1,2;3,4})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 2.0 (v wb 0 1)))
    (is (= 3.0 (v wb 0 2)))
    (is (= 4.0 (v wb 0 3)))))

(deftest toCol-flatten
  (let [wb (mk [0 0 "=TOCOL({1,2;3,4})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 4.0 (v wb 3 0)))))

(deftest wraprows-pad
  (let [wb (mk [0 0 "=WRAPROWS({1,2,3,4,5}, 2, 0)"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 2.0 (v wb 0 1)))
    (is (= 5.0 (v wb 2 0)))
    (is (= 0.0 (v wb 2 1)))))

;; ---------------------------------------------------------------------------
;; Filter / sort

(deftest unique-removes-dupes
  (let [wb (mk [0 0 "=UNIQUE({1;1;2;3;3;3})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 2.0 (v wb 1 0)))
    (is (= 3.0 (v wb 2 0)))))

(deftest sort-ascending
  (let [wb (mk [0 0 "=SORT({3;1;2})"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 2.0 (v wb 1 0)))
    (is (= 3.0 (v wb 2 0)))))

(deftest sort-descending
  (let [wb (mk [0 0 "=SORT({3;1;2},1,-1)"])]
    (is (= 3.0 (v wb 0 0)))
    (is (= 2.0 (v wb 1 0)))
    (is (= 1.0 (v wb 2 0)))))

(deftest filter-keeps-matching-rows
  (let [wb (-> (e/empty-workbook)
               (e/set-cell (c/pack 0 0 0) 10) (e/set-cell (c/pack 0 0 1) "yes")
               (e/set-cell (c/pack 0 1 0) 20) (e/set-cell (c/pack 0 1 1) "no")
               (e/set-cell (c/pack 0 2 0) 30) (e/set-cell (c/pack 0 2 1) "yes")
               (e/set-cell (c/pack 0 5 0) "=FILTER(A1:A3, B1:B3=\"yes\")")
               e/recalc)]
    (is (= 10.0 (:v (e/get-cell wb (c/pack 0 5 0)))))
    (is (= 30.0 (:v (e/get-cell wb (c/pack 0 6 0)))))))

;; ---------------------------------------------------------------------------
;; LAMBDA helpers

(deftest map-doubles
  (let [wb (mk [0 0 "=MAP({1,2,3}, LAMBDA(x, x*2))"])]
    (is (= 2.0 (v wb 0 0)))
    (is (= 4.0 (v wb 0 1)))
    (is (= 6.0 (v wb 0 2)))))

(deftest map-multi-array
  (let [wb (mk [0 0 "=MAP({1,2,3}, {10,20,30}, LAMBDA(a, b, a*b))"])]
    (is (= 10.0  (v wb 0 0)))
    (is (= 40.0  (v wb 0 1)))
    (is (= 90.0  (v wb 0 2)))))

(deftest reduce-sum
  (let [wb (mk [0 0 "=REDUCE(0, {1,2,3,4,5}, LAMBDA(a,v, a+v))"])]
    (is (= 15.0 (v wb 0 0)))))

(deftest scan-prefix-sum
  (let [wb (mk [0 0 "=SCAN(0, {1,2,3,4}, LAMBDA(a,v, a+v))"])]
    (is (= 1.0  (v wb 0 0)))
    (is (= 3.0  (v wb 0 1)))
    (is (= 6.0  (v wb 0 2)))
    (is (= 10.0 (v wb 0 3)))))

(deftest byrow-sums
  (let [wb (mk [0 0 "=BYROW({1,2,3;4,5,6}, LAMBDA(r, SUM(r)))"])]
    (is (= 6.0  (v wb 0 0)))
    (is (= 15.0 (v wb 1 0)))))

(deftest bycol-maxes
  (let [wb (mk [0 0 "=BYCOL({1,2;3,4}, LAMBDA(c, MAX(c)))"])]
    (is (= 3.0 (v wb 0 0)))
    (is (= 4.0 (v wb 0 1)))))

(deftest makearray-multiplication-table
  (let [wb (mk [0 0 "=MAKEARRAY(3,3, LAMBDA(r,c, r*c))"])]
    (is (= 1.0 (v wb 0 0)))
    (is (= 6.0 (v wb 1 2)))
    (is (= 9.0 (v wb 2 2)))))

;; ---------------------------------------------------------------------------
;; #CALC! nested-array detection (Excel: body returning multi-cell array
;; in MAP / SCAN / BYROW / BYCOL / MAKEARRAY)

(deftest map-nested-array-errors
  (let [wb (mk [0 0 "=MAP({1,2}, LAMBDA(x, {x,x}))"])]
    (is (= :calc (:v (at wb 0 0))))))

(deftest byrow-nested-array-errors
  ;; Body returns the row itself (a 1xN area) instead of collapsing
  (let [wb (mk [0 0 "=BYROW({1,2;3,4}, LAMBDA(r, r))"])]
    (is (= :calc (:v (at wb 0 0))))))

(deftest bycol-nested-array-errors
  (let [wb (mk [0 0 "=BYCOL({1,2;3,4}, LAMBDA(c, c))"])]
    (is (= :calc (:v (at wb 0 0))))))

(deftest makearray-nested-array-errors
  (let [wb (mk [0 0 "=MAKEARRAY(2, 2, LAMBDA(r, c, {1,2}))"])]
    (is (= :calc (:v (at wb 0 0))))))

(deftest scan-nested-array-errors
  (let [wb (mk [0 0 "=SCAN(0, {1,2,3}, LAMBDA(a,v, {v,v}))"])]
    (is (= :calc (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; 1x1 area auto-unwrap — when a lambda body is `c*10` and `c` is a
;; 1x1 sub-area, broadcasting yields a 1x1 area; helpers collapse it
;; back to a scalar so the result remains a flat 1D array.

(deftest bycol-on-1d-row-unwraps-1x1
  ;; BYCOL on a 1xN row: each "column" is a 1x1 area. Body multiplies
  ;; by 10, broadcast keeps it as 1x1, scalarize unwraps.
  (let [wb (mk [0 0 "=BYCOL({1,2,3}, LAMBDA(c, c*10))"])]
    (is (= 10.0 (v wb 0 0)))
    (is (= 20.0 (v wb 0 1)))
    (is (= 30.0 (v wb 0 2)))))

(deftest byrow-on-1d-col-unwraps-1x1
  (let [wb (mk [0 0 "=BYROW({1;2;3}, LAMBDA(r, r*10))"])]
    (is (= 10.0 (v wb 0 0)))
    (is (= 20.0 (v wb 1 0)))
    (is (= 30.0 (v wb 2 0)))))

(deftest map-on-1d-array-doesnt-spread
  ;; MAP body returning x*1 is scalar — nothing to unwrap, but
  ;; verify nothing breaks with the new scalarize wrapper.
  (let [wb (mk [0 0 "=MAP({5,10,15}, LAMBDA(x, x+1))"])]
    (is (= 6.0  (v wb 0 0)))
    (is (= 11.0 (v wb 0 1)))
    (is (= 16.0 (v wb 0 2)))))

;; ---------------------------------------------------------------------------
;; FORMULATEXT round-trips for new fns

(deftest formulatext-roundtrips-array-fns
  (let [wb (mk [0 0 "=SEQUENCE(3)"]
               [4 0 "=FORMULATEXT(A1)"]
               [4 1 "=BYROW({1,2;3,4},LAMBDA(r,SUM(r)))"]
               [5 1 "=FORMULATEXT(B5)"])]
    (is (= "=SEQUENCE(3)" (:v (at wb 4 0))))
    (is (= "=BYROW({1,2;3,4},LAMBDA(r,SUM(r)))" (:v (at wb 5 1))))))
