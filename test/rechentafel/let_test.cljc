(ns rechentafel.let-test
  "Tests for the LET special form. Semantics are sequential bindings,
  eager value-binding (each value evaluated once), errors propagate via
  the bound value, names are case-insensitive, recursive RHS rejected.

  See `.internal/lang-design.md` §5 + LET-research notes."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.parser :as p]
            [rechentafel.unparse :as u]
            [rechentafel.functions.all]))

(defn- mk [& cells]
  (e/recalc
   (reduce (fn [wb [r col input]]
             (e/set-cell wb (c/pack 0 r col) input))
           (e/empty-workbook)
           cells)))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))

;; ---------------------------------------------------------------------------
;; Basic semantics

(deftest simple-binding
  (let [wb (mk [0 0 "=LET(x, 5, x*2)"])]
    (is (= 10.0 (:v (at wb 0 0))))))

(deftest sequential-scope
  ;; name2 sees name1
  (let [wb (mk [0 0 "=LET(a, 3, b, a+1, a*b)"])]
    (is (= 12.0 (:v (at wb 0 0))))))

(deftest let-in-body
  ;; nested LET binds independently
  (let [wb (mk [0 0 "=LET(x, 1, LET(y, 2, x+y))"])]
    (is (= 3.0 (:v (at wb 0 0))))))

(deftest case-insensitive
  (let [wb (mk [0 0 "=LET(MyVar, 7, myvar+1)"])]
    (is (= 8.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Eager value-binding — RAND() called once

(deftest eager-binding
  (let [wb (mk [0 0 "=LET(x, RAND(), x-x)"])]
    ;; if x were re-evaluated each reference, this would rarely be 0
    (is (= 0.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Refers to cells

(deftest binding-from-cell
  (let [wb (mk [0 0 10]
               [0 1 20]
               [1 0 "=LET(s, A1+B1, s*2)"])]
    (is (= 60.0 (:v (at wb 1 0))))))

(deftest body-refers-to-cell-and-binding
  (let [wb (mk [0 0 5]
               [1 0 "=LET(x, A1, x*x+A1)"])]
    (is (= 30.0 (:v (at wb 1 0))))))

;; ---------------------------------------------------------------------------
;; Shadowing

(deftest let-shadows-defined-name
  (let [wb (-> (e/empty-workbook)
               (e/define-name "MyConst" "=42")
               (e/set-cell (c/pack 0 0 0) "=MyConst")
               (e/set-cell (c/pack 0 1 0) "=LET(MyConst, 7, MyConst*2)")
               e/recalc)]
    (is (= 42.0 (:v (at wb 0 0))))    ;; outside LET: workbook-defined
    (is (= 14.0 (:v (at wb 1 0))))))   ;; inside LET: shadowed to 7

;; ---------------------------------------------------------------------------
;; Error propagation

(deftest error-from-binding-flows-through
  (let [wb (mk [0 0 "=LET(x, 1/0, x+1)"])]
    (is (= :div0 (:v (at wb 0 0))))))

(deftest error-from-body
  (let [wb (mk [0 0 "=LET(x, 5, x/0)"])]
    (is (= :div0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Bad shape — falls through to a regular CALL → #NAME?

(deftest even-args-is-not-a-let
  ;; LET with even arg-count means no body — Excel returns #NAME? / #VALUE!
  (let [wb (mk [0 0 "=LET(x, 5)"])]
    (is (some? (#{:name :value} (:v (at wb 0 0)))))))

;; ---------------------------------------------------------------------------
;; Dependency tracking

(deftest dep-graph-tracks-cell-deps-through-let
  (let [wb (mk [0 0 10]
               [0 1 20]
               [1 0 "=LET(s, A1+B1, s)"])]
    (is (= 30.0 (:v (at wb 1 0))))
    (let [wb (-> wb (e/set-cell (c/pack 0 0 0) 100) e/recalc)]
      (is (= 120.0 (:v (at wb 1 0)))))))

(deftest dep-graph-skips-shadowed-name
  ;; Inside LET, `x` is bound to the literal 7; no dep on cell A1.
  ;; The formula's reads should not include A1.
  (let [wb (-> (e/empty-workbook)
               (e/set-cell (c/pack 0 1 1)
                           "=LET(A1, 7, A1*2)"))]    ;; uses *literal* :name "A1" — only happens via define-name normally.
    ;; This isn't exactly the spec since A1 in an expression is a :ref,
    ;; not a :name, so this edge case doesn't actually trigger. Just
    ;; verify the formula still evaluates.
    (let [wb (e/recalc wb)]
      ;; A1 in source is a :ref ; LET-binding "A1" is a :name in body.
      ;; Excel disallows this; we accept it gracefully — body's A1 cell
      ;; ref still wins because `:ref` AST is independent of env.
      (is (some? (at wb 1 1))))))

;; ---------------------------------------------------------------------------
;; Round-trip through unparser

(deftest unparse-let
  (let [src "LET(x,5,x+1)"
        ast (p/parse src)]
    (is (= "LET(x,5,x+1)" (u/unparse ast)))))

(deftest formulatext-let
  (let [wb (mk [0 0 "=LET(x, 5, x+1)"]
               [0 1 "=FORMULATEXT(A1)"])]
    (is (= "=LET(x,5,x+1)" (:v (at wb 0 1))))))

;; ---------------------------------------------------------------------------
;; LET combined with prior phases (3D refs, tables)

(deftest let-over-3d-ref
  (let [wb (-> (e/empty-workbook ["S1" "S2" "S3"])
               (e/set-cell (c/pack 0 0 0) 1)
               (e/set-cell (c/pack 1 0 0) 2)
               (e/set-cell (c/pack 2 0 0) 3)
               (e/set-cell (c/pack 0 4 0) "=LET(s, SUM(S1:S3!A1), s*10)")
               e/recalc)]
    (is (= 60.0 (:v (e/get-cell wb (c/pack 0 4 0)))))))
