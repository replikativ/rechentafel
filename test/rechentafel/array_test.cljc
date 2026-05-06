(ns rechentafel.array-test
  "M1 array broadcasting + scalar lifting tests.

  Covers the array-design-v2.md test matrix:
    - element-wise binops over scalar / area / area×area
    - row × column broadcast → 2D
    - error propagation per cell
    - shape mismatch → #VALUE!
    - scalar fns lift transparently when registered :lift? true
      (n1/n2/n1-guarded helpers in math.cljc auto-attach :scalar?
       metadata which register! turns into :lift? true)"
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
(defn- vals-of [v] (mapv (fn [row] (mapv :v row)) (:values v)))

;; ---------------------------------------------------------------------------
;; Binop broadcasting

(deftest scalar-times-array
  (let [wb (mk [0 0 "=SUM({1,2,3}*2)"])]
    (is (= 12.0 (:v (at wb 0 0))))))

(deftest array-plus-array-same-shape
  (let [wb (mk [0 0 "=SUM({1,2,3}+{10,20,30})"])]
    (is (= 66.0 (:v (at wb 0 0))))))

(deftest array-times-array-with-cells
  (let [wb (-> (e/empty-workbook)
               (e/set-cell (c/pack 0 0 0) 1)
               (e/set-cell (c/pack 0 1 0) 2)
               (e/set-cell (c/pack 0 2 0) 3)
               (e/set-cell (c/pack 0 0 1) 10)
               (e/set-cell (c/pack 0 1 1) 20)
               (e/set-cell (c/pack 0 2 1) 30)
               (e/set-cell (c/pack 0 4 0) "=SUM(A1:A3*B1:B3)")
               e/recalc)]
    ;; 1*10 + 2*20 + 3*30 = 140
    (is (= 140.0 (:v (e/get-cell wb (c/pack 0 4 0)))))))

(deftest row-vs-column-vector-broadcast
  ;; {1;2} + {10,20} → 2x2 matrix:
  ;; [11 21]
  ;; [12 22]
  (let [wb (mk [0 0 "=SUM({1;2}+{10,20})"])]
    (is (= 66.0 (:v (at wb 0 0))))))

(deftest shape-mismatch-yields-value-error
  (let [wb (mk [0 0 "=SUM({1,2,3}+{10,20})"])]
    ;; SUM of #VALUE! propagates the error
    (is (= :value (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Error propagation per cell

(deftest error-propagates-per-cell
  ;; {1,#DIV/0!,3}+1 spills 3 cells across A1,B1,C1. The middle cell
  ;; carries #DIV/0! independently of its siblings.
  (let [wb (mk [0 0 "={1,#DIV/0!,3}+1"])]
    (is (= 2.0      (:v (at wb 0 0))))
    (is (= :div0    (:v (at wb 0 1))))
    (is (= 4.0      (:v (at wb 0 2))))))

;; ---------------------------------------------------------------------------
;; Scalar function lifting

(deftest sqrt-lifts
  (let [wb (mk [0 0 "=SUM(SQRT({1,4,9,16}))"])]
    (is (= 10.0 (:v (at wb 0 0))))))

(deftest abs-lifts
  (let [wb (mk [0 0 "=SUM(ABS({-1,-2,3,-4}))"])]
    (is (= 10.0 (:v (at wb 0 0))))))

(deftest two-arg-scalar-fn-lifts
  ;; POWER is registered via n2; broadcasts both args
  (let [wb (mk [0 0 "=SUM(POWER({2,3,4}, 2))"])]
    ;; 4 + 9 + 16 = 29
    (is (= 29.0 (:v (at wb 0 0))))))

(deftest scalar-and-array-arg-broadcast
  ;; MOD takes scalar + scalar, but broadcast both
  (let [wb (mk [0 0 "=SUM(MOD({10,11,12}, 3))"])]
    ;; 1 + 2 + 0 = 3
    (is (= 3.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Comparison binops broadcast too

(deftest comparison-broadcasts
  (let [wb (mk [0 0 "=SUM(--({1,2,3,4} > 2))"])]
    ;; {FALSE, FALSE, TRUE, TRUE} as numbers → 2
    (is (= 2.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Cells from a real range × a real range

(deftest range-times-range
  (let [wb (-> (e/empty-workbook)
               (e/set-cell (c/pack 0 0 0) 2)
               (e/set-cell (c/pack 0 1 0) 3)
               (e/set-cell (c/pack 0 0 1) 5)
               (e/set-cell (c/pack 0 1 1) 7)
               (e/set-cell (c/pack 0 4 0) "=SUMPRODUCT(A1:A2, B1:B2)")
               (e/set-cell (c/pack 0 5 0) "=SUM(A1:A2 * B1:B2)")  ;; same answer via broadcast
               e/recalc)]
    (is (= 31.0 (:v (e/get-cell wb (c/pack 0 4 0)))))
    (is (= 31.0 (:v (e/get-cell wb (c/pack 0 5 0)))))))

;; ---------------------------------------------------------------------------
;; Aggregates still work on areas (regression check — they're :lift? false)

(deftest aggregates-not-lifted
  (let [wb (mk [0 0 "=SUM({1,2,3,4,5})"]
               [0 1 "=AVERAGE({2,4,6})"]
               [0 2 "=COUNT({1,2,3,4})"]
               [0 3 "=MAX({3,1,2})"]
               [0 4 "=MIN({3,1,2})"])]
    (is (= 15.0 (:v (at wb 0 0))))
    (is (= 4.0  (:v (at wb 0 1))))
    (is (= 4.0  (:v (at wb 0 2))))
    (is (= 3.0  (:v (at wb 0 3))))
    (is (= 1.0  (:v (at wb 0 4))))))

;; ---------------------------------------------------------------------------
;; Array result top-level: stored value is the top-left (M1 fallback;
;; M2 spill will fix this so it spills into siblings).

(deftest array-at-cell-spills
  ;; M2: array result fills siblings.  ={1,2,3}*2 spills A1:C1.
  (let [wb (mk [0 0 "={1,2,3}*2"])]
    (is (= 2.0 (:v (at wb 0 0))))
    (is (= 4.0 (:v (at wb 0 1))))
    (is (= 6.0 (:v (at wb 0 2))))))

;; ---------------------------------------------------------------------------
;; @ operator (implicit intersection)

(deftest at-operator-collapses-to-current-row
  (let [wb (-> (e/empty-workbook)
               (e/set-cell (c/pack 0 0 0) 10)
               (e/set-cell (c/pack 0 1 0) 20)
               (e/set-cell (c/pack 0 2 0) 30)
               (e/set-cell (c/pack 0 1 1) "=@A1:A3+1")  ;; in row 1 → A2+1 = 21
               (e/set-cell (c/pack 0 2 1) "=@A1:A3+1")  ;; in row 2 → A3+1 = 31
               e/recalc)]
    (is (= 21.0 (:v (e/get-cell wb (c/pack 0 1 1)))))
    (is (= 31.0 (:v (e/get-cell wb (c/pack 0 2 1)))))))

(deftest at-on-1x1-area-passthrough
  (let [wb (mk [0 0 "=@(1+2)"])]
    (is (= 3.0 (:v (at wb 0 0))))))

(deftest at-roundtrip
  (let [wb (mk [0 0 "=@A1:A10"]
               [0 1 "=FORMULATEXT(A1)"])]
    (is (= "=@A1:A10" (:v (at wb 0 1))))))

;; ---------------------------------------------------------------------------
;; M2: spill — array-result formula fills sibling cells

(deftest array-spills-into-row
  (let [wb (mk [0 0 "={10,20,30}"])]
    (is (= 10.0 (:v (at wb 0 0))))
    (is (= 20.0 (:v (at wb 0 1))))
    (is (= 30.0 (:v (at wb 0 2))))))

(deftest array-spills-into-column
  (let [wb (mk [0 0 "={10;20;30}"])]
    (is (= 10.0 (:v (at wb 0 0))))
    (is (= 20.0 (:v (at wb 1 0))))
    (is (= 30.0 (:v (at wb 2 0))))))

(deftest array-2d-spills-rectangle
  (let [wb (mk [0 0 "={1,2;3,4}"])]
    (is (= 1.0 (:v (at wb 0 0))))
    (is (= 2.0 (:v (at wb 0 1))))
    (is (= 3.0 (:v (at wb 1 0))))
    (is (= 4.0 (:v (at wb 1 1))))))

(deftest spill-blocked-by-occupied-cell
  ;; B1 already has a value; A1={1,2,3} would want B1 → #SPILL!.
  (let [wb (mk [0 1 7]                    ;; B1 = 7
               [0 0 "={10,20,30}"])]      ;; A1 wants A1:C1
    (is (= :spill (:v (at wb 0 0))))
    (is (= 7.0    (:v (at wb 0 1))))      ;; B1 untouched
    (is (= :blank (:t (at wb 0 2))))))    ;; C1 untouched (blank)

(deftest spill-recovers-when-block-cleared
  (let [wb1 (mk [0 1 7]
                [0 0 "={10,20,30}"])
        wb2 (-> wb1 (e/set-cell (c/pack 0 0 1) nil) e/recalc)]
    (is (= :spill (:v (at wb1 0 0))))
    (is (= 10.0 (:v (at wb2 0 0))))
    (is (= 20.0 (:v (at wb2 0 1))))
    (is (= 30.0 (:v (at wb2 0 2))))))

(deftest spill-shape-shrink-clears-orphans
  (let [wb1 (mk [0 0 "={10,20,30,40,50}"])
        wb2 (-> wb1 (e/set-cell (c/pack 0 0 0) "={1,2,3}") e/recalc)]
    (is (= 50.0 (:v (at wb1 0 4))))
    (is (= 1.0  (:v (at wb2 0 0))))
    (is (= 3.0  (:v (at wb2 0 2))))
    ;; D1, E1 got cleared
    (is (= :blank (:t (at wb2 0 3))))
    (is (= :blank (:t (at wb2 0 4))))))

(deftest spill-anchor-replaced-by-scalar-clears-siblings
  (let [wb1 (mk [0 0 "={10,20,30}"])
        wb2 (-> wb1 (e/set-cell (c/pack 0 0 0) "=99") e/recalc)]
    (is (= 99.0   (:v (at wb2 0 0))))
    (is (= :blank (:t (at wb2 0 1))))
    (is (= :blank (:t (at wb2 0 2))))))

(deftest sequence-spills
  (let [wb (mk [0 0 "=ROW(A1:A5)"])]
    ;; ROW(A1:A5) returns {1;2;3;4;5} in array context — well, ROW is
    ;; a tricky case. We test SEQUENCE in M3a; this just shows the
    ;; binop-driven array spills.
    nil))

;; ---------------------------------------------------------------------------
;; A1# (ANCHORARRAY) operator

(deftest spill-ref-resolves-to-current-shape
  (let [wb (mk [0 0 "={10,20,30}"]
               [3 0 "=SUM(A1#)"])]
    (is (= 60.0 (:v (at wb 3 0))))))

(deftest spill-ref-tracks-shape-changes
  (let [wb1 (mk [0 0 "={10,20,30}"]
                [3 0 "=SUM(A1#)"])
        wb2 (-> wb1 (e/set-cell (c/pack 0 0 0) "={100,200,300,400}") e/recalc)]
    (is (= 60.0   (:v (at wb1 3 0))))
    (is (= 1000.0 (:v (at wb2 3 0))))))

(deftest spill-ref-on-non-anchor-is-ref-error
  (let [wb (mk [0 0 99]
               [0 1 "=A1#"])]
    (is (= :ref (:v (at wb 0 1))))))

(deftest spill-ref-roundtrip
  (let [wb (mk [0 0 "={1,2,3}"]
               [3 0 "=A1#"]
               [3 1 "=FORMULATEXT(A4)"])]
    (is (= "=A1#" (:v (at wb 3 1))))))

;; ---------------------------------------------------------------------------
;; Per-module broadcasting smoke tests for fns marked :lift? true.
;; Each module's pure-scalar fns should broadcast across array args.

;; --- text module ---

(deftest text-upper-lifts
  (let [wb (mk [0 0 "=UPPER({\"a\",\"b\",\"c\"})"])]
    (is (= "A" (:v (at wb 0 0))))
    (is (= "B" (:v (at wb 0 1))))
    (is (= "C" (:v (at wb 0 2))))))

(deftest text-len-lifts
  (let [wb (mk [0 0 "=SUM(LEN({\"a\",\"bb\",\"ccc\"}))"])]
    (is (= 6.0 (:v (at wb 0 0))))))

;; --- datetime module (cljc via cljc.java-time) ---

(deftest datetime-year-lifts
  (let [wb (mk [0 0 "=YEAR({DATE(2020,1,1),DATE(2021,6,15),DATE(2022,12,31)})"])]
    (is (= 2020.0 (:v (at wb 0 0))))
    (is (= 2021.0 (:v (at wb 0 1))))
    (is (= 2022.0 (:v (at wb 0 2))))))

(deftest datetime-date-lifts
  ;; DATE with 3 scalar args should broadcast over arrays
  (let [wb (mk [0 0 "=YEAR(DATE({2020,2021,2022}, 1, 1))"])]
    (is (= 2020.0 (:v (at wb 0 0))))
    (is (= 2021.0 (:v (at wb 0 1))))
    (is (= 2022.0 (:v (at wb 0 2))))))

;; --- engineering module ---

(deftest engineering-bin2dec-lifts
  (let [wb (mk [0 0 "=BIN2DEC({\"10\",\"100\",\"1000\"})"])]
    (is (= 2.0 (:v (at wb 0 0))))
    (is (= 4.0 (:v (at wb 0 1))))
    (is (= 8.0 (:v (at wb 0 2))))))

(deftest engineering-bitand-lifts
  (let [wb (mk [0 0 "=SUM(BITAND({3,5,7}, 1))"])]
    ;; 1 + 1 + 1 = 3
    (is (= 3.0 (:v (at wb 0 0))))))

;; --- financial module ---

(deftest financial-fv-lifts
  ;; broadcast rate over an array
  (let [wb (mk [0 0 "=FV({0.05, 0.10}, 10, -100, 0)"])]
    (is (< 1257.0 (:v (at wb 0 0)) 1259.0))
    (is (< 1593.0 (:v (at wb 0 1)) 1595.0))))

(deftest financial-pmt-lifts
  (let [wb (mk [0 0 "=PMT(0.05, {12, 24}, 1000)"])]
    ;; first ~ -112.83, second ~ -72.47
    (is (< -113.0 (:v (at wb 0 0)) -112.0))
    (is (< -73.0 (:v (at wb 0 1)) -72.0))))

;; --- stats module ---

(deftest stats-norm-dist-lifts
  ;; NORM.S.DIST with array of z-scores
  (let [wb (mk [0 0 "=NORM.S.DIST({-1, 0, 1}, TRUE)"])]
    (is (< 0.158 (:v (at wb 0 0)) 0.159))
    (is (< 0.499 (:v (at wb 0 1)) 0.501))
    (is (< 0.841 (:v (at wb 0 2)) 0.842))))

(deftest stats-binom-dist-lifts
  ;; BINOM.DIST broadcasting k across an array
  (let [wb (mk [0 0 "=SUM(BINOM.DIST({0,1,2,3,4,5}, 5, 0.5, FALSE))"])]
    ;; cdf over all values sums to 1
    (is (< 0.999 (:v (at wb 0 0)) 1.001))))

(deftest stats-fisher-lifts
  (let [wb (mk [0 0 "=FISHER({0, 0.5})"])]
    (is (< -0.001 (:v (at wb 0 0)) 0.001))
    (is (< 0.549 (:v (at wb 0 1)) 0.550))))

;; --- math module (already covered by sqrt-lifts/abs-lifts/etc., add a couple more) ---

(deftest math-fact-lifts
  (let [wb (mk [0 0 "=SUM(FACT({1,2,3,4}))"])]
    ;; 1 + 2 + 6 + 24 = 33
    (is (= 33.0 (:v (at wb 0 0))))))

(deftest math-int-lifts
  (let [wb (mk [0 0 "=SUM(INT({1.5, 2.7, 3.1}))"])]
    ;; 1 + 2 + 3 = 6
    (is (= 6.0 (:v (at wb 0 0))))))

(deftest math-round-lifts
  (let [wb (mk [0 0 "=SUM(ROUND({1.234, 2.567, 3.891}, 1))"])]
    ;; 1.2 + 2.6 + 3.9 = 7.7
    (is (< 7.69 (:v (at wb 0 0)) 7.71))))

;; --- logical module ---

(deftest logical-not-lifts
  (let [wb (mk [0 0 "=SUM(--NOT({TRUE,FALSE,TRUE}))"])]
    ;; FALSE + TRUE + FALSE = 1
    (is (= 1.0 (:v (at wb 0 0))))))

;; --- misc module ---

(deftest misc-address-lifts
  (let [wb (mk [0 0 "=ADDRESS({1,2}, {1,2})"])]
    (is (= "$A$1" (:v (at wb 0 0))))
    (is (= "$B$2" (:v (at wb 0 1))))))
