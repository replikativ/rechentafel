(ns rechentafel.eval-test
  "Tests for the evaluator + dep graph + recalc loop."
  (:require [clojure.test :refer [deftest is testing]]
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))       ;; register all fns

(defn- n [x] {:t :num :v (double x)})
(defn- s [x] {:t :str :v x})

(defn- mk [& cells]
  (e/recalc
   (reduce (fn [wb [row col input]]
             (e/set-cell wb (c/pack 0 row col) input))
           (e/empty-workbook)
           cells)))

(defn- at [wb r c] (e/get-cell wb (c/pack 0 r c)))

;; ---------------------------------------------------------------------------
;; Literals + scalar refs

(deftest literals-and-scalars
  (let [wb (mk [0 0 10] [0 1 "hello"] [0 2 true])]
    (is (= (n 10)    (at wb 0 0)))
    (is (= (s "hello") (at wb 0 1)))
    (is (= {:t :bool :v true} (at wb 0 2)))))

(deftest scalar-arithmetic
  (let [wb (mk [0 0 7] [0 1 3] [0 2 "=A1+B1"] [0 3 "=A1*B1"] [0 4 "=A1/B1"])]
    (is (= (n 10) (at wb 0 2)))
    (is (= (n 21) (at wb 0 3)))
    (is (= (n (/ 7.0 3.0)) (at wb 0 4)))))

(deftest div-by-zero-propagates
  (let [wb (mk [0 0 5] [0 1 0] [0 2 "=A1/B1"] [0 3 "=C1+1"])]
    (is (= {:t :err :v :div0} (at wb 0 2)))
    (is (= {:t :err :v :div0} (at wb 0 3)))))

;; ---------------------------------------------------------------------------
;; Ranges + aggregate fns

(deftest sum-over-range
  (let [wb (mk [0 0 1] [0 1 2] [0 2 3] [0 3 4] [0 4 5]
               [1 0 "=SUM(A1:E1)"])]
    (is (= (n 15) (at wb 1 0)))))

(deftest average-min-max
  (let [wb (mk [0 0 10] [1 0 20] [2 0 30]
               [0 1 "=AVERAGE(A1:A3)"]
               [1 1 "=MIN(A1:A3)"]
               [2 1 "=MAX(A1:A3)"])]
    (is (= (n 20) (at wb 0 1)))
    (is (= (n 10) (at wb 1 1)))
    (is (= (n 30) (at wb 2 1)))))

;; ---------------------------------------------------------------------------
;; Edit propagation

(deftest edit-dirty-propagation
  (let [wb (mk [0 0 1] [0 1 "=A1*2"] [0 2 "=B1+10"] [0 3 "=SUM(A1:C1)"])]
    (is (= (n 1) (at wb 0 0)))
    (is (= (n 2) (at wb 0 1)))
    (is (= (n 12) (at wb 0 2)))
    (is (= (n 15) (at wb 0 3)))
    (let [wb' (e/set-and-recalc wb (c/pack 0 0 0) 5)]
      (is (= (n 5)  (at wb' 0 0)))
      (is (= (n 10) (at wb' 0 1)))
      (is (= (n 20) (at wb' 0 2)))
      (is (= (n 35) (at wb' 0 3))))))

(deftest edit-inside-range
  (let [wb (mk [0 0 1] [1 0 2] [2 0 3] [3 0 4] [4 0 5]
               [0 1 "=SUM(A1:A5)"])]
    (is (= (n 15) (at wb 0 1)))
    (is (= (n 112) (at (e/set-and-recalc wb (c/pack 0 2 0) 100) 0 1)))))

;; ---------------------------------------------------------------------------
;; Cycle detection

(deftest direct-cycle-ref-error
  (let [wb (mk [0 0 "=B1"] [0 1 "=A1"])]
    (is (= {:t :err :v :ref} (at wb 0 0)))
    (is (= {:t :err :v :ref} (at wb 0 1)))))

(deftest indirect-cycle-ref-error
  (let [wb (mk [0 0 "=B1+1"] [0 1 "=C1+1"] [0 2 "=A1+1"])]
    (is (= {:t :err :v :ref} (at wb 0 0)))
    (is (= {:t :err :v :ref} (at wb 0 1)))
    (is (= {:t :err :v :ref} (at wb 0 2)))))

;; ---------------------------------------------------------------------------
;; Formula → literal → formula churn

(deftest formula-replaced-by-literal-then-formula
  (let [wb (mk [0 0 10] [0 1 20] [0 2 "=A1+B1"])]
    (is (= (n 30) (at wb 0 2)))
    (let [wb1 (e/set-and-recalc wb (c/pack 0 0 2) 999)]     ;; overwrite formula with literal
      (is (= (n 999) (at wb1 0 2)))
      (let [wb2 (e/set-and-recalc wb1 (c/pack 0 0 2) "=A1*B1")]  ;; back to formula
        (is (= (n 200) (at wb2 0 2)))))))

;; ---------------------------------------------------------------------------
;; R1C1 interning — identical filled-down formulas share a single AST

(deftest shared-formula-interning
  (let [wb (reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 0) (inc i))) (e/empty-workbook) (range 20))
        wb (reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 1) (str "=A" (inc i) "*2"))) wb (range 20))
        wb (e/recalc wb)]
    (is (= (n 2)  (at wb 0 1)))
    (is (= (n 40) (at wb 19 1)))
    (let [asts (vals (select-keys (:formulas wb)
                                  (map #(c/pack 0 % 1) (range 20))))]
      (is (= 1 (count (set asts))) "all 20 B-column formulas dedupe to 1 RC form")
      (is (= 1 (count (set (map #(System/identityHashCode %) asts))))
          "and share the same object (interned)"))))

;; ---------------------------------------------------------------------------
;; Multi-sheet workbooks — cross-sheet refs, ranges, dep invalidation, #REF!

(defn- mks [sheet-names & cells]
  (e/recalc
   (reduce (fn [wb [sheet row col input]]
             (e/set-cell wb (c/pack sheet row col) input))
           (e/empty-workbook sheet-names)
           cells)))

(deftest cross-sheet-scalar-ref
  (let [wb (mks ["Sheet1" "Sheet2"]
                [0 0 0 10]
                [1 0 0 20]
                [0 1 0 "=Sheet2!A1+A1"])]
    (is (= (n 30) (e/get-cell wb (c/pack 0 1 0))))))

(deftest cross-sheet-range
  (let [wb (mks ["Sheet1" "Sheet2"]
                [1 0 0 1] [1 1 0 2] [1 2 0 3]
                [0 0 0 "=SUM(Sheet2!A1:A3)"]
                [0 1 0 "=SUM(Sheet2!A:A)"])]
    (is (= (n 6) (e/get-cell wb (c/pack 0 0 0))))
    (is (= (n 6) (e/get-cell wb (c/pack 0 1 0))))))

(deftest cross-sheet-dep-invalidation
  (let [wb  (mks ["Sheet1" "Sheet2"]
                 [0 0 0 10]
                 [1 0 0 20]
                 [0 1 0 "=Sheet2!A1+A1"])
        _   (is (= (n 30) (e/get-cell wb (c/pack 0 1 0))))
        wb' (e/set-and-recalc wb (c/pack 1 0 0) 100)]
    (is (= (n 110) (e/get-cell wb' (c/pack 0 1 0))))))

(deftest cross-sheet-transitive
  (testing "A→B chain where B lives on another sheet"
    (let [wb  (mks ["Sheet1" "Sheet2"]
                   [0 0 0 1]
                   [1 0 0 "=Sheet1!A1*10"]    ;; Sheet2!A1 = Sheet1!A1*10
                   [0 1 0 "=Sheet2!A1+5"])    ;; Sheet1!A2 = Sheet2!A1+5
          _   (is (= (n 15) (e/get-cell wb (c/pack 0 1 0))))
          wb' (e/set-and-recalc wb (c/pack 0 0 0) 7)]
      (is (= (n 75) (e/get-cell wb' (c/pack 0 1 0)))))))

(deftest unknown-sheet-is-ref-error
  (let [wb (mks ["Sheet1"]
                [0 0 0 "=Nope!A1"]
                [0 1 0 "=SUM(Nope!A1:A3)"])]
    (is (= :ref (:v (e/get-cell wb (c/pack 0 0 0)))))
    (is (= :ref (:v (e/get-cell wb (c/pack 0 1 0)))))))

;; ---------------------------------------------------------------------------
;; Named ranges — workbook-level defined names resolve inside formulas and
;; dep-graph invalidates dependents when cells inside the named range change.

(deftest named-range-scalar
  (let [wb (-> (e/empty-workbook)
               (e/define-name "MyVal" "A1")
               (e/set-cell (c/pack 0 0 0) 10)
               (e/set-cell (c/pack 0 0 1) "=MyVal")
               e/recalc)]
    (is (= (n 10) (at wb 0 1)))))

(deftest named-range-sum
  (let [wb (-> (e/empty-workbook)
               (e/define-name "Data" "A1:A3")
               (e/set-cell (c/pack 0 0 0) 1)
               (e/set-cell (c/pack 0 1 0) 2)
               (e/set-cell (c/pack 0 2 0) 3)
               (e/set-cell (c/pack 0 0 1) "=SUM(Data)")
               e/recalc)]
    (is (= (n 6) (at wb 0 1)))))

(deftest named-range-formula-target
  (testing "name can resolve to an arbitrary formula AST, not just a ref"
    (let [wb (-> (e/empty-workbook)
                 (e/define-name "Double" "=A1*2")
                 (e/set-cell (c/pack 0 0 0) 21)
                 (e/set-cell (c/pack 0 0 1) "=Double")
                 e/recalc)]
      (is (= (n 42) (at wb 0 1))))))

(deftest named-range-case-insensitive
  (let [wb (-> (e/empty-workbook)
               (e/define-name "MyVal" "A1")
               (e/set-cell (c/pack 0 0 0) 7)
               (e/set-cell (c/pack 0 0 1) "=myval+MYVAL+MyVal")
               e/recalc)]
    (is (= (n 21) (at wb 0 1)))))

(deftest named-range-dep-invalidation
  (testing "editing a cell inside a named range dirties consumers of the name"
    (let [wb  (-> (e/empty-workbook)
                  (e/define-name "Data" "A1:A3")
                  (e/set-cell (c/pack 0 0 0) 1)
                  (e/set-cell (c/pack 0 1 0) 2)
                  (e/set-cell (c/pack 0 2 0) 3)
                  (e/set-cell (c/pack 0 0 1) "=SUM(Data)")
                  e/recalc)
          _   (is (= (n 6) (at wb 0 1)))
          wb' (e/set-and-recalc wb (c/pack 0 1 0) 100)]
      (is (= (n 104) (at wb' 0 1))))))

(deftest undefined-name-is-name-error
  (let [wb (-> (e/empty-workbook)
               (e/set-cell (c/pack 0 0 0) "=Nope")
               e/recalc)]
    (is (= {:t :err :v :name} (at wb 0 0)))))

;; ---------------------------------------------------------------------------
;; Chain performance smoke — 1000 dependent cells recalc after head edit

(deftest ^:perf linear-chain-recalc
  (let [len 1000
        wb (reduce (fn [wb i]
                     (e/set-cell wb (c/pack 0 i 0)
                                 (if (zero? i) 0 (str "=A" i "+1"))))
                   (e/empty-workbook) (range len))
        wb (e/recalc wb)]
    (is (= (n (double (dec len))) (at wb (dec len) 0)))
    (let [wb' (e/set-and-recalc wb (c/pack 0 0 0) 42)]
      (is (= (n (+ 42.0 (dec len))) (at wb' (dec len) 0))))))
