(ns rechentafel.sheet-context-test
  "An UNQUALIFIED ref resolves against the sheet that CONTAINS the formula.

  `=SUM(B1:B2)` written on `Model` means `Model!B1:B2` — never `Sheet1!B1:B2`.
  This is the single most common shape in a real workbook (you rarely qualify
  a ref to your own sheet), and it used to resolve against the workbook's
  global `:cur-sheet`, which nothing varies per cell. Every unqualified ref on
  every sheet but the first therefore read sheet 0 — silently, with plausible
  numbers, which is the worst way to be wrong.

  Both halves of the engine have to agree on the owning sheet:
  the EVALUATOR (what value comes out) and the DEP GRAPH (what recalcs when
  an input moves). Each is asserted below."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- model
  "Inputs (sheet 0): B1 = growth, B2 = base.
   Model  (sheet 1): B1 = base*(1+growth)   — qualified refs
                     B2 = B1*(1+Inputs!B1)  — MIXED: B1 is unqualified
                     B3 = SUM(B1:B2)        — unqualified range"
  []
  (-> (e/empty-workbook ["Inputs" "Model"])
      (e/set-cell (c/pack 0 0 1) 0.10)
      (e/set-cell (c/pack 0 1 1) 1000)
      (e/set-cell (c/pack 1 0 1) "=Inputs!B2*(1+Inputs!B1)")
      (e/set-cell (c/pack 1 1 1) "=B1*(1+Inputs!B1)")
      (e/set-cell (c/pack 1 2 1) "=SUM(B1:B2)")
      (e/recalc)))

(deftest unqualified-ref-resolves-against-the-owning-sheet
  (let [wb (model)]
    (testing "a qualified ref was always fine"
      (is (= {:t :num :v 1100.0} (e/get-cell wb (c/pack 1 0 1)))))
    (testing "an unqualified CELL ref means my sheet — Model!B1, not Inputs!B1"
      ;; the bug returned 0.11 here: it read Inputs!B1 (0.10) instead of
      ;; Model!B1 (1100)
      (is (= {:t :num :v 1210.0} (e/get-cell wb (c/pack 1 1 1)))))
    (testing "an unqualified RANGE means my sheet — Model!B1:B2"
      ;; the bug returned 1000.1: SUM(Inputs!B1:B2) = 0.10 + 1000
      (is (= {:t :num :v 2310.0} (e/get-cell wb (c/pack 1 2 1)))))))

(deftest dep-edges-point-at-the-owning-sheet
  (let [wb (model)
        reads (get (:reads wb) (c/pack 1 2 1))]
    (testing "SUM(B1:B2) on Model reads MODEL's B1:B2"
      (is (= #{{:sheet 1 :r0 0 :r1 1 :c0 1 :c1 1}} reads)))
    (testing "so an edit propagates along the right edges: growth 10% → 5%"
      (let [wb (e/set-and-recalc wb (c/pack 0 0 1) 0.05)]
        (is (= {:t :num :v 1050.0} (e/get-cell wb (c/pack 1 0 1))))
        (is (= {:t :num :v 1102.5} (e/get-cell wb (c/pack 1 1 1))))
        (is (= {:t :num :v 2152.5} (e/get-cell wb (c/pack 1 2 1))))))
    (testing "and an edit to the formula's OWN sheet propagates too"
      ;; Model!B1 is a formula; overwrite it with a literal and B2/B3 must move
      (let [wb (e/set-and-recalc wb (c/pack 1 0 1) 100)]
        (is (= {:t :num :v 110.00000000000001} (e/get-cell wb (c/pack 1 1 1))))
        (is (= {:t :num :v 210.0} (e/get-cell wb (c/pack 1 2 1))))))))

(deftest cur-sheet-still-serves-cell-less-evaluation
  (testing ":cur-sheet remains the fallback when there is no owning cell —
            a single-sheet workbook is unaffected, and so is a workbook whose
            active sheet is set explicitly"
    (let [wb (-> (e/empty-workbook ["A" "B"])
                 (assoc :cur-sheet 1)
                 (e/set-cell (c/pack 1 0 0) 7)
                 (e/set-cell (c/pack 0 0 0) 99)
                 (e/recalc))]
      ;; a formula ON sheet B resolves A1 against B (its own sheet), which here
      ;; coincides with :cur-sheet
      (let [wb (e/set-and-recalc wb (c/pack 1 1 0) "=A1*2")]
        (is (= {:t :num :v 14.0} (e/get-cell wb (c/pack 1 1 0)))))
      ;; …and a formula on sheet A resolves A1 against A, NOT against :cur-sheet
      (let [wb (e/set-and-recalc wb (c/pack 0 1 0) "=A1*2")]
        (is (= {:t :num :v 198.0} (e/get-cell wb (c/pack 0 1 0))))))))
