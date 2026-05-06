(ns rechentafel.lambda-test
  "Tests for LAMBDA, ISOMITTED, and named-LAMBDA UDFs.

  Coverage:
    - basic LAMBDA(x, body)(arg) application
    - closure capture via LET / outer LAMBDA
    - named LAMBDAs as user-defined functions through `define-name`
    - ISOMITTED for optional params via `[Y]` syntax
    - error cases — too few / too many args, recursion cap
    - round-trip via unparser

  Spec source: Microsoft Support docs + lang-design.md §6."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.parser :as p]
            [rechentafel.unparse :as u]
            [rechentafel.value :as v]
            [rechentafel.functions.all]))

(defn- mk [& cells]
  (e/recalc
   (reduce (fn [wb [r col input]]
             (e/set-cell wb (c/pack 0 r col) input))
           (e/empty-workbook)
           cells)))

(defn- at [wb r col] (e/get-cell wb (c/pack 0 r col)))

;; ---------------------------------------------------------------------------
;; Basic application

(deftest lambda-immediate
  (let [wb (mk [0 0 "=LAMBDA(x, x*2)(5)"])]
    (is (= 10.0 (:v (at wb 0 0))))))

(deftest lambda-multi-param
  (let [wb (mk [0 0 "=LAMBDA(x, y, x+y)(3, 4)"])]
    (is (= 7.0 (:v (at wb 0 0))))))

(deftest lambda-no-params
  (let [wb (mk [0 0 "=LAMBDA(42)()"])]
    (is (= 42.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Closure capture

(deftest closure-over-let
  (let [wb (mk [0 0 "=LET(x, 5, LAMBDA(y, x+y)(3))"])]
    (is (= 8.0 (:v (at wb 0 0))))))

(deftest closure-over-outer-lambda
  ;; LAMBDA(x, LAMBDA(y, x+y))(3)(4) → (3 + 4) = 7
  (let [wb (mk [0 0 "=LAMBDA(x, LAMBDA(y, x+y))(3)(4)"])]
    (is (= 7.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Named LAMBDAs as UDFs

(deftest named-lambda-udf
  (let [wb (-> (e/empty-workbook)
               (e/define-name "Double" "=LAMBDA(x, x*2)")
               (e/set-cell (c/pack 0 0 0) "=Double(7)")
               e/recalc)]
    (is (= 14.0 (:v (at wb 0 0))))))

(deftest named-lambda-recursive
  ;; Factorial — classic recursive LAMBDA pattern.
  (let [wb (-> (e/empty-workbook)
               (e/define-name "Fact" "=LAMBDA(n, IF(n<=1, 1, n*Fact(n-1)))")
               (e/set-cell (c/pack 0 0 0) "=Fact(5)")
               e/recalc)]
    (is (= 120.0 (:v (at wb 0 0))))))

(deftest named-lambda-shadows-builtin
  ;; A defined name overrides a registered fn (Excel semantics).
  (let [wb (-> (e/empty-workbook)
               (e/define-name "SUM" "=LAMBDA(a, b, a-b)")    ;; intentional override
               (e/set-cell (c/pack 0 0 0) "=SUM(10, 3)")     ;; → 7, not 13
               e/recalc)]
    (is (= 7.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Optional params + ISOMITTED

(deftest isomitted-with-arg-supplied
  ;; LAMBDA(x, [y], IF(ISOMITTED(y), x, x+y))(3, 4) → 7
  (let [wb (mk [0 0 "=LAMBDA(x, [y], IF(ISOMITTED(y), x, x+y))(3, 4)"])]
    (is (= 7.0 (:v (at wb 0 0))))))

(deftest isomitted-with-arg-omitted
  ;; LAMBDA(x, [y], IF(ISOMITTED(y), x*10, x+y))(3) → 30
  (let [wb (mk [0 0 "=LAMBDA(x, [y], IF(ISOMITTED(y), x*10, x+y))(3)"])]
    (is (= 30.0 (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; Errors

(deftest too-few-args-required
  (let [wb (mk [0 0 "=LAMBDA(x, y, x+y)(3)"])]
    (is (= :value (:v (at wb 0 0))))))

(deftest too-many-args
  (let [wb (mk [0 0 "=LAMBDA(x, x*2)(3, 4)"])]
    (is (= :value (:v (at wb 0 0))))))

(deftest application-of-non-lambda
  (let [wb (mk [0 0 "=LET(x, 5, x(7))"])]
    (is (= :value (:v (at wb 0 0))))))

;; ---------------------------------------------------------------------------
;; LAMBDA + cells

(deftest lambda-body-references-cells
  (let [wb (mk [0 0 10]
               [0 1 20]
               [1 0 "=LAMBDA(k, A1*k+B1)(3)"])]
    (is (= 50.0 (:v (at wb 1 0))))))

;; ---------------------------------------------------------------------------
;; Round-trip via FORMULATEXT

(deftest formulatext-lambda
  (let [wb (mk [0 0 "=LAMBDA(x, x*2)(5)"]
               [0 1 "=FORMULATEXT(A1)"])]
    (is (= "=LAMBDA(x,x*2)(5)" (:v (at wb 0 1))))))

(deftest formulatext-named-lambda-call
  (let [wb (-> (e/empty-workbook)
               (e/define-name "Double" "=LAMBDA(x, x*2)")
               (e/set-cell (c/pack 0 0 0) "=Double(5)")
               (e/set-cell (c/pack 0 0 1) "=FORMULATEXT(A1)")
               e/recalc)]
    ;; Canonical form uppercases all call names — same as registered fns.
    (is (= "=DOUBLE(5)" (:v (at wb 0 1))))))

;; ---------------------------------------------------------------------------
;; LAMBDA combined with other features

(deftest lambda-over-3d-ref
  (let [wb (-> (e/empty-workbook ["S1" "S2" "S3"])
               (e/set-cell (c/pack 0 0 0) 1)
               (e/set-cell (c/pack 1 0 0) 2)
               (e/set-cell (c/pack 2 0 0) 3)
               (e/set-cell (c/pack 0 4 0) "=LAMBDA(rng, SUM(rng))(S1:S3!A1)")
               e/recalc)]
    (is (= 6.0 (:v (e/get-cell wb (c/pack 0 4 0)))))))

(deftest unparse-lambda-roundtrip
  (let [src "LAMBDA(x,[y],IF(ISOMITTED(y),x,x+y))"
        ast (p/parse src)]
    (is (= src (u/unparse ast)))))
