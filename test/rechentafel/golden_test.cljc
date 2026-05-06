(ns rechentafel.golden-test
  "Cross-runtime correctness regression. Runs the benchmark shapes at
  a few canonical sizes and asserts the result cell matches an
  analytical expected value. The same numbers were independently
  confirmed by Apache POI and LibreOffice (see
  .internal/bench-results.md), so this test acts as a permanent
  regression guard against any change to recalc / aggregation /
  spill / lambda evaluation that would shift these results."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- chain-wb [n]
  (reduce (fn [wb i]
            (e/set-cell wb (c/pack 0 i 0)
                        (if (zero? i) 0 (str "=A" i "+1"))))
          (e/empty-workbook) (range n)))

(defn- wide-wb [n]
  (let [wb (reduce (fn [w i] (e/set-cell w (c/pack 0 i 0) (double i)))
                   (e/empty-workbook) (range n))]
    (reduce (fn [w i] (e/set-cell w (c/pack 0 i 1) (str "=A" (inc i) "*2")))
            wb (range n))))

(defn- aggregate-wb [n]
  (let [wb (reduce (fn [w i] (e/set-cell w (c/pack 0 i 0) (double i)))
                   (e/empty-workbook) (range n))]
    (e/set-cell wb (c/pack 0 0 1) (str "=SUM(A1:A" n ")"))))

(defn- star-wb [n]
  (reduce (fn [w i] (e/set-cell w (c/pack 0 i 1) (str "=A1*" (inc i))))
          (e/set-cell (e/empty-workbook) (c/pack 0 0 0) 100)
          (range n)))

(defn- spill-wb [n]
  (e/set-cell (e/empty-workbook) (c/pack 0 0 0) (str "=SEQUENCE(" n ")")))

(defn- lambda-rec-wb [n]
  (-> (e/empty-workbook)
      (e/define-name "Fact" "=LAMBDA(k, IF(k<=1, 1, k*Fact(k-1)))")
      (e/set-cell (c/pack 0 0 0) (str "=Fact(" n ")"))))

(defn- v [wb r col] (:v (e/get-cell wb (c/pack 0 r col))))

;; ---------------------------------------------------------------------------
;; Each test runs at multiple N to catch off-by-ones / scaling bugs.

(deftest chain-results
  (doseq [n [10 100 1000 10000]]
    (testing (str "n=" n)
      (let [wb (e/recalc (chain-wb n))]
        (is (= (double (dec n)) (v wb (dec n) 0)))))))

(deftest wide-results
  (doseq [n [10 100 1000]]
    (testing (str "n=" n)
      (let [wb (e/recalc (wide-wb n))]
        (is (= (double (* 2 (dec n))) (v wb (dec n) 1)))))))

(deftest aggregate-results
  (doseq [n [10 100 1000 10000]]
    (testing (str "n=" n)
      (let [wb (e/recalc (aggregate-wb n))]
        (is (= (double (/ (* (long n) (dec (long n))) 2))
               (v wb 0 1)))))))

(deftest star-results
  (doseq [n [10 100 1000]]
    (testing (str "n=" n)
      (let [wb (e/recalc (star-wb n))]
        (is (= (double (* 100 n)) (v wb (dec n) 1)))))))

(deftest spill-results
  (doseq [n [10 100 1000]]
    (testing (str "n=" n)
      (let [wb (e/recalc (spill-wb n))]
        (is (= (double n) (v wb (dec n) 0)))))))

(deftest lambda-recursion-results
  ;; Verified across JVM, cljs (V8), and Excel-equivalent reference
  ;; impls. IEEE 754 makes these bit-for-bit reproducible.
  (testing "Fact(5)"
    (let [wb (e/recalc (lambda-rec-wb 5))]
      (is (= 120.0 (v wb 0 0)))))
  (testing "Fact(10)"
    (let [wb (e/recalc (lambda-rec-wb 10))]
      (is (= 3628800.0 (v wb 0 0)))))
  (testing "Fact(20)"
    (let [wb (e/recalc (lambda-rec-wb 20))]
      ;; 20! exceeds JS safe-integer (2^53), but still representable
      ;; as a double — both runtimes give the same approximation.
      (is (= 2.43290200817664E18 (v wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Cross-engine values cached from a real run on this machine. These
;; were emitted by:
;;   - LibreOffice (snap soffice) via --convert-to xlsx, read back via POI
;;   - Apache POI's FormulaEvaluator
;;   - spread-clj on JVM and cljs
;; All four agreed to >12 significant digits.

(def ^:private cross-engine-expectations
  "Recorded values from chain/aggregate at N=5k, 10k, where LO + POI +
  spread-clj all agreed. Acts as a hard-coded reference: changes to
  the engine that move these numbers should be intentional and obvious."
  {:chain     {5000  4999.0
               10000 9999.0}
   :aggregate {5000  12497500.0
               10000 49995000.0}})

(deftest cross-engine-cached
  (testing "results match reference values from LibreOffice + POI runs"
    (doseq [[n exp] (:chain cross-engine-expectations)]
      (let [wb (e/recalc (chain-wb n))]
        (is (= exp (v wb (dec n) 0))
            (str "chain n=" n))))
    (doseq [[n exp] (:aggregate cross-engine-expectations)]
      (let [wb (e/recalc (aggregate-wb n))]
        (is (= exp (v wb 0 1))
            (str "aggregate n=" n))))))
