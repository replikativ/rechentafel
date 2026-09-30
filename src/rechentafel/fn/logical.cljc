(ns rechentafel.fn.logical
  "Logical functions (POI category: logical — 11 fns).

  Split between strict (TRUE, FALSE, NOT, AND, OR, CHOOSE) and lazy
  (IF, IFERROR, IFNA, IFS, SWITCH).

  Lazy functions receive `(ctx ast-args)` from `call-lazy`. They force
  individual child ASTs through `(:eval ctx)` — the evaluator supplies
  this hook so lazy fns don't have to know how evaluation is wired.
  Without `:eval`, the fns fall back to treating each ast-arg as already
  an evaluated value (so they still work in unit tests that pre-eval)."
  (:require [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Strict helpers

(f/register! "TRUE"  (fn [_args] val/TRUE)  :arity [0 0])
(f/register! "FALSE" (fn [_args] val/FALSE) :arity [0 0])

(f/register! "NOT"
             ^{:scalar? true}
             (fn [args] (val/boolean-v (not (f/bool! (nth args 0)))))
             :arity [1 1])

;; AND / OR semantics follow POI's BooleanFunction:
;;   - Walk every scalar arg (areas expand).
;;   - Strings and blanks are silently ignored (not coerced).
;;   - Errors propagate immediately (strict call already short-circuits
;;     top-level error args; inside an area we propagate the first error).
;;   - If no truthy/boolean was found AT ALL, Excel returns #VALUE!.

(defn- boolean-reduce
  "Iterate scalars for AND/OR; `op` is an accumulator:
   `(op state value)` returns new state. `init` is the starting state.
   After iteration, `finalize` turns state → result or throws."
  [args init op finalize]
  (let [state (volatile! init)
        seen? (volatile! false)]
    (f/each-scalar
     args
     (fn [v _in-area?]
       (case (:t v)
         :err   (f/domain-error! (:v v))
         :bool  (do (vreset! seen? true)
                    (vswap! state op (:v v)))
         :num   (do (vreset! seen? true)
                    (vswap! state op (not (zero? (:v v)))))
         ;; blank, str: ignored
         nil)))
    (if @seen? (finalize @state) (f/domain-error! :value))))

(f/register! "AND"
             (fn [args]
               (val/boolean-v
                (boolean-reduce args true (fn [s v] (and s v)) identity)))
             :arity [1 nil])

(f/register! "OR"
             (fn [args]
               (val/boolean-v
                (boolean-reduce args false (fn [s v] (or s v)) identity)))
             :arity [1 nil])

(f/register! "XOR"  ;; POI has XOR too though I skipped from doc; harmless add.
             (fn [args]
               (val/boolean-v
                (boolean-reduce args false (fn [s v] (not= s v)) identity)))
             :arity [1 nil])

;; CHOOSE can be strict — the evaluator passes all args evaluated, and we
;; just pick. (Excel semantically is lazy but real-world impls evaluate
;; all branches; POI itself lists CHOOSE as NotImplemented.)
(f/register! "CHOOSE"
             (fn [args]
               (let [idx (long (f/num! (nth args 0)))
                     n   (dec (count args))]
                 (when (or (< idx 1) (> idx n)) (f/domain-error! :value))
                 (nth args idx)))
             :arity [2 nil])

;; ---------------------------------------------------------------------------
;; Lazy impls.
;;
;; Contract: the evaluator passes `ctx` with `:eval` → `(fn [ctx ast] value)`
;; that forces a single AST branch. Lazy fns never evaluate unneeded branches.
;;
;; For unit testing before the evaluator exists, we support a fallback path:
;; if `:eval` is missing and an ast-arg already looks like a value (has `:t`
;; tag in the `#{:num :str :bool :blank :err :ref :area}` set), use it as-is.

(defn- eval1 [ctx ast]
  (if-let [ev (:eval ctx)]
    (ev ctx ast)
    ;; fallback for tests: treat ast as an already-evaluated value
    ast))

(defn- pick-if
  "One element of an array IF: the condition's error, else then or else."
  [[c t e]]
  (cond (val/err? c) c
        (val/truthy? c) t
        :else e))

(f/register! "IF"
             ;; A scalar condition picks one branch and evaluates only that.
             ;; An array condition (IF(A1:C1=A2:C2, ...)) is element-wise, as
             ;; in Excel: both branches are evaluated and each element takes
             ;; its own, scalars broadcast.
             (fn [ctx ast-args]
               (let [cond-v (eval1 ctx (nth ast-args 0))
                     then-v #(if (>= (count ast-args) 2) (eval1 ctx (nth ast-args 1)) val/TRUE)
                     else-v #(if (>= (count ast-args) 3) (eval1 ctx (nth ast-args 2)) val/FALSE)]
                 (cond
                   (f/area? cond-v) (f/lift-call pick-if [cond-v (then-v) (else-v)])
                   (val/err? cond-v) cond-v
                   (val/truthy? cond-v) (then-v)
                   :else (else-v))))
             :arity [2 3] :lazy? true)

(defn- replace-errors
  "Array `v` with each element `hit?` replaced by the alternative at its
   position (`alt` an array: by position, a single row or column repeated;
   else the value itself). Not `lift-call`, which propagates an element's
   error before the replacement can see it."
  [v alt hit?]
  (let [avs (when (f/area? alt) (:values alt))
        at (fn [i j]
             (if avs
               (let [i' (if (= 1 (count avs)) 0 i) j' (if (= 1 (count (first avs))) 0 j)]
                 (get-in avs [i' j'] val/ERR-NA))
               alt))]
    (-> (dissoc v :sheet :ref-r0 :ref-r1 :ref-c0 :ref-c1)
        (update :values (fn [rows]
                          (vec (map-indexed (fn [i row]
                                              (vec (map-indexed (fn [j x] (if (hit? x) (at i j) x)) row)))
                                            rows)))))))

(f/register! "IFERROR"
             ;; element-wise over an array value, as IF
             (fn [ctx ast-args]
               (let [v (eval1 ctx (nth ast-args 0))]
                 (cond
                   (f/area? v) (replace-errors v (eval1 ctx (nth ast-args 1)) val/err?)
                   (val/err? v) (eval1 ctx (nth ast-args 1))
                   :else v)))
             :arity [2 2] :lazy? true)

(f/register! "IFNA"
             (fn [ctx ast-args]
               (let [v (eval1 ctx (nth ast-args 0))
                     na? #(and (val/err? %) (= :na (:v %)))]
                 (cond
                   (f/area? v) (replace-errors v (eval1 ctx (nth ast-args 1)) na?)
                   (na? v) (eval1 ctx (nth ast-args 1))
                   :else v)))
             :arity [2 2] :lazy? true)

(f/register! "IFS"
  ;; IFS(cond1, v1, cond2, v2, ...) — return first vN whose condN is truthy.
  ;; If none match, POI returns #N/A.
             (fn [ctx ast-args]
               (when (odd? (count ast-args)) (f/domain-error! :na))
               (loop [pairs ast-args]
                 (if (empty? pairs) val/ERR-NA
                     (let [c (eval1 ctx (first pairs))]
                       (cond
                         (val/err? c) c
                         (val/truthy? c) (eval1 ctx (second pairs))
                         :else (recur (drop 2 pairs)))))))
             :arity [2 nil] :lazy? true)

(f/register! "SWITCH"
  ;; SWITCH(expr, match1, v1, match2, v2, ..., [default])
             (fn [ctx ast-args]
               (let [expr (eval1 ctx (nth ast-args 0))
                     rest-args (drop 1 ast-args)
                     has-default? (odd? (count rest-args))
                     default-ast (when has-default? (last rest-args))
                     pairs (if has-default? (butlast rest-args) rest-args)]
                 (if (val/err? expr) expr
                     (loop [ps pairs]
                       (if (empty? ps)
                         (if has-default? (eval1 ctx default-ast) val/ERR-NA)
                         (let [m (eval1 ctx (first ps))]
                           (cond
                             (val/err? m) m
                             (= (:v expr) (:v m))  ;; simple equality on tagged values
                             (eval1 ctx (second ps))
                             :else (recur (drop 2 ps)))))))))
             :arity [3 nil] :lazy? true)
