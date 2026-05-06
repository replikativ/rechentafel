(ns rechentafel.fn.math
  "Mathematical functions (POI category: math — 61 fns).

  Every entry mirrors POI's NumericFunction / AggregateFunction / Matrix-
  Function / ATP semantics. The file is organised roughly by shape:
  unary numeric, binary numeric, rounding, aggregates, matrix, and the
  few oddballs (ROMAN/ARABIC/SUBTOTAL/AGGREGATE).

  Each public registration calls `f/register!`; the module has no other
  side-effects, so `(require 'rechentafel.fn.math)` is enough to install
  them."
  (:require [clojure.string :as str]
            [rechentafel.platform :as p]
            [rechentafel.rng :as rng]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Unary-numeric helpers

(defn- n1
  "Wraps a function `(double -> double)` as a strict 1-arg Excel fn.
  The result is checked for NaN / Infinity (domain error → #NUM!).
  Attaches `:scalar?` metadata so `register!` enables broadcasting.

  Uses an explicit `with-meta` rather than the `^{...}` reader form
  because the latter compiles to invalid JS in the current cljs
  release when applied to a fn-form returned from another fn."
  [op]
  (with-meta
    (fn [args]
      (val/number (f/check-num! (op (f/num! (nth args 0))))))
    {:scalar? true}))

(defn- n1-guarded
  "Variant of `n1` where the input must satisfy `ok?`; otherwise #NUM!."
  [op ok?]
  (with-meta
    (fn [args]
      (let [x (f/num! (nth args 0))]
        (when-not (ok? x) (f/domain-error! :num))
        (val/number (f/check-num! (op x)))))
    {:scalar? true}))

(defn- n2
  "Wraps a function `(double double -> double)` as a strict 2-arg fn."
  [op]
  (with-meta
    (fn [args]
      (val/number (f/check-num! (op (f/num! (nth args 0))
                                    (f/num! (nth args 1))))))
    {:scalar? true}))

;; ---------------------------------------------------------------------------
;; Simple scalar math (unary)

(f/register! "ABS"  (n1 #(Math/abs (double %)))  :arity [1 1])
(f/register! "SIGN" ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (val/number (cond (pos? x) 1.0 (neg? x) -1.0 :else 0.0))))
             :arity [1 1])

(f/register! "SQRT"   (n1-guarded #(Math/sqrt (double %)) #(>= (double %) 0.0)) :arity [1 1])
(f/register! "EXP"    (n1 #(Math/exp (double %))) :arity [1 1])
(f/register! "LN"     (n1-guarded #(Math/log (double %)) #(> (double %) 0.0)) :arity [1 1])
(f/register! "LOG10"  (n1-guarded #(Math/log10 (double %)) #(> (double %) 0.0)) :arity [1 1])

(f/register! "PI"     (fn [_args] (val/number Math/PI)) :arity [0 0])

(f/register! "SIN"  (n1 #(Math/sin (double %)))  :arity [1 1])
(f/register! "COS"  (n1 #(Math/cos (double %)))  :arity [1 1])
(f/register! "TAN"  (n1 #(Math/tan (double %)))  :arity [1 1])

(f/register! "ASIN" (n1-guarded #(Math/asin (double %))
                                #(<= -1.0 (double %) 1.0)) :arity [1 1])
(f/register! "ACOS" (n1-guarded #(Math/acos (double %))
                                #(<= -1.0 (double %) 1.0)) :arity [1 1])
(f/register! "ATAN" (n1 #(Math/atan (double %))) :arity [1 1])

(f/register! "SINH" (n1 #(Math/sinh (double %))) :arity [1 1])
(f/register! "COSH" (n1 #(Math/cosh (double %))) :arity [1 1])
(f/register! "TANH" (n1 #(Math/tanh (double %))) :arity [1 1])

(f/register! "ASINH" (n1 (fn [^double x]
                           (Math/log (+ x (Math/sqrt (inc (* x x)))))))
             :arity [1 1])
(f/register! "ACOSH" (n1-guarded (fn [^double x]
                                   (Math/log (+ x (Math/sqrt (dec (* x x))))))
                                 #(>= (double %) 1.0))
             :arity [1 1])
(f/register! "ATANH" (n1-guarded (fn [^double x]
                                   (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x)))))
                                 #(< -1.0 (double %) 1.0))
             :arity [1 1])

(f/register! "DEGREES" (n1 (fn [^double x] (* x (/ 180.0 Math/PI)))) :arity [1 1])
(f/register! "RADIANS" (n1 (fn [^double x] (* x (/ Math/PI 180.0)))) :arity [1 1])

;; Reciprocal trig (Excel 2013+). Poles at sin/tan/sinh/tanh = 0 raise
;; #DIV/0! in Excel (not #NUM!). Cross-checked with IronCalc.
(defn- div0-at-zero [f zero-test]
  (with-meta
    (fn [args]
      (let [x (f/num! (nth args 0))]
        (when (zero-test x) (f/domain-error! :div0))
        (val/number (f/check-num! (f x)))))
    {:scalar? true}))

(f/register! "COT"  (div0-at-zero (fn [^double x] (/ 1.0 (Math/tan x)))
                                  (fn [^double x] (zero? (Math/tan x))))
             :arity [1 1])
(f/register! "COTH" (div0-at-zero (fn [^double x] (/ 1.0 (Math/tanh x)))
                                  (fn [^double x] (zero? x)))
             :arity [1 1])
(f/register! "CSC"  (div0-at-zero (fn [^double x] (/ 1.0 (Math/sin x)))
                                  (fn [^double x] (zero? (Math/sin x))))
             :arity [1 1])
(f/register! "CSCH" (div0-at-zero (fn [^double x] (/ 1.0 (Math/sinh x)))
                                  (fn [^double x] (zero? x)))
             :arity [1 1])
(f/register! "SEC"  (n1 (fn [^double x] (/ 1.0 (Math/cos x))))  :arity [1 1])
(f/register! "SECH" (n1 (fn [^double x] (/ 1.0 (Math/cosh x)))) :arity [1 1])
(f/register! "ACOT"
  ;; Principal branch in (0, π): atan(1/x) for x>0, π + atan(1/x) for x<0, π/2 for x=0.
             (n1 (fn [^double x]
                   (cond (zero? x) (/ Math/PI 2.0)
                         (pos? x)  (Math/atan (/ 1.0 x))
                         :else     (+ Math/PI (Math/atan (/ 1.0 x))))))
             :arity [1 1])
(f/register! "ACOTH"
  ;; Defined for |x|>1: 0.5*ln((x+1)/(x-1)).
             (n1-guarded (fn [^double x] (* 0.5 (Math/log (/ (+ x 1.0) (- x 1.0)))))
                         #(> (Math/abs (double %)) 1.0))
             :arity [1 1])

;; GAUSS / PHI — standard-normal helpers.
;; PHI(x) = density φ(x) = (1/√(2π)) e^{-x²/2}
;; GAUSS(x) = Φ(x) − 0.5 (cumulative, centered at 0)
(let [inv-sqrt-2pi (/ 1.0 (Math/sqrt (* 2.0 Math/PI)))
      inv-sqrt-2   (/ 1.0 (Math/sqrt 2.0))
      erf (fn [^double x]
            ;; Abramowitz & Stegun 7.1.26
            (let [sign (if (neg? x) -1.0 1.0)
                  ax   (Math/abs x)
                  t    (/ 1.0 (+ 1.0 (* 0.3275911 ax)))
                  y    (- 1.0
                          (* t (+ 0.254829592
                                  (* t (+ -0.284496736
                                          (* t (+ 1.421413741
                                                  (* t (+ -1.453152027
                                                          (* t 1.061405429))))))))
                             (Math/exp (- (* ax ax)))))]
              (* sign y)))]
  (f/register! "PHI"
               (n1 (fn [^double x] (* inv-sqrt-2pi (Math/exp (- (/ (* x x) 2.0))))))
               :arity [1 1])
  (f/register! "GAUSS"
               (n1 (fn [^double x] (* 0.5 (erf (* x inv-sqrt-2)))))
               :arity [1 1]))

;; ---------------------------------------------------------------------------
;; Binary scalar math

(f/register! "POWER"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     y (f/num! (nth args 1))
                     r (Math/pow x y)]
                 (val/number (f/check-num! r))))
             :arity [2 2])

(f/register! "ATAN2"
  ;; Excel convention: ATAN2(x, y) returns atan(y/x). POI and java.lang.Math
  ;; take (y, x) — we flip here.
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     y (f/num! (nth args 1))]
                 (when (and (zero? x) (zero? y)) (f/domain-error! :div0))
                 (val/number (Math/atan2 y x))))
             :arity [2 2])

(f/register! "LOG"
             ^{:scalar? true}
             (fn [args]
               (let [x    (f/num! (nth args 0))
                     base (if (> (count args) 1) (f/num! (nth args 1)) 10.0)]
                 (when-not (pos? x) (f/domain-error! :num))
                 (when (or (<= base 0.0) (== base 1.0)) (f/domain-error! :num))
                 (val/number (f/check-num! (/ (Math/log x) (Math/log base))))))
             :arity [1 2])

(f/register! "MOD"
  ;; Sign follows divisor: MOD(-5, 3) = 1. POI uses floor semantics:
  ;;    n - d * floor(n/d)
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     d (f/num! (nth args 1))]
                 (when (zero? d) (f/domain-error! :div0))
                 (val/number (- n (* d (Math/floor (/ n d)))))))
             :arity [2 2])

(f/register! "QUOTIENT"
  ;; INT(n / d) toward zero
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     d (f/num! (nth args 1))]
                 (when (zero? d) (f/domain-error! :div0))
                 (val/number (double (long (/ n d))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; Rounding — POI's NumericFunction.ROUND family.

(defn- round-away-zero ^double [^double x]
  (if (neg? x)
    (- (Math/floor (+ (- x) 0.5)))
    (Math/floor (+ x 0.5))))

(defn- scale-round ^double [op ^double n ^long digits]
  (let [scale (Math/pow 10.0 digits)]
    (/ (op (* n scale)) scale)))

(f/register! "ROUND"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     d (long (f/num! (nth args 1)))]
                 (val/number (scale-round round-away-zero n d))))
             :arity [2 2])

(f/register! "ROUNDUP"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     d (long (f/num! (nth args 1)))]
                 (val/number (scale-round (fn [^double x]
                                            (if (neg? x) (- (Math/ceil (- x)))
                                                (Math/ceil x))) n d))))
             :arity [2 2])

(f/register! "ROUNDDOWN"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     d (long (f/num! (nth args 1)))]
                 (val/number (scale-round (fn [^double x]
                                            (if (neg? x) (- (Math/floor (- x)))
                                                (Math/floor x))) n d))))
             :arity [2 2])

(f/register! "INT"
  ;; Excel INT is mathematical floor (toward -∞), NOT truncate.
             ^{:scalar? true}
             (fn [args] (val/number (Math/floor (f/num! (nth args 0)))))
             :arity [1 1])

(f/register! "TRUNC"
  ;; Truncate toward zero, optional digits arg.
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     d (long (if (> (count args) 1) (f/num! (nth args 1)) 0))
                     sc (Math/pow 10.0 d)
                     t  (* sc n)]
                 (val/number (/ (if (neg? t) (Math/ceil t) (Math/floor t)) sc))))
             :arity [1 2])

(defn- ceiling-match-sign
  "POI CEILING: sign(number) must equal sign(significance) or be zero.
  Returns the rounded value away from zero to the nearest multiple."
  [^double n ^double s]
  (cond
    (zero? s) 0.0
    (zero? n) 0.0
    (and (neg? n) (pos? s)) (f/domain-error! :num)
    (and (pos? n) (neg? s)) (f/domain-error! :num)
    :else (let [abs-n (Math/abs n)
                abs-s (Math/abs s)
                r     (* abs-s (Math/ceil (/ abs-n abs-s)))]
            (if (neg? n) (- r) r))))

(f/register! "CEILING"
             ^{:scalar? true}
             (fn [args] (val/number (ceiling-match-sign (f/num! (nth args 0))
                                                        (f/num! (nth args 1)))))
             :arity [2 2])

(defn- floor-match-sign
  [^double n ^double s]
  (cond
    (zero? s) (if (zero? n) 0.0 (f/domain-error! :div0))
    (zero? n) 0.0
    (and (neg? n) (pos? s)) (f/domain-error! :num)
    (and (pos? n) (neg? s)) (f/domain-error! :num)
    :else (let [abs-n (Math/abs n)
                abs-s (Math/abs s)
                r     (* abs-s (Math/floor (/ abs-n abs-s)))]
            (if (neg? n) (- r) r))))

(f/register! "FLOOR"
             ^{:scalar? true}
             (fn [args] (val/number (floor-match-sign (f/num! (nth args 0))
                                                      (f/num! (nth args 1)))))
             :arity [2 2])

;; CEILING.MATH(number, [significance=1], [mode=0])
;;   mode = 0: for negative numbers round toward zero (ceiling)
;;   mode != 0: for negative numbers round away from zero
(f/register! "CEILING.MATH"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     s (if (> (count args) 1) (f/num! (nth args 1))
                           (if (neg? n) -1.0 1.0))
                     m (if (> (count args) 2) (f/num! (nth args 2)) 0.0)]
                 (if (zero? s) (val/number 0.0)
                     (let [abs-s (Math/abs s)]
                       (val/number
                        (cond
                          (zero? n) 0.0
                          (pos? n) (* abs-s (Math/ceil (/ n abs-s)))
                          (zero? m) (* abs-s (Math/ceil (/ n abs-s)))      ;; toward zero
                          :else     (* (- abs-s) (Math/ceil (/ (- n) abs-s)))))))))
             :arity [1 3])

;; CEILING.PRECISE(number, [significance]) — always toward +∞, sign(sig) ignored.
(f/register! "CEILING.PRECISE"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     s (if (> (count args) 1) (f/num! (nth args 1)) 1.0)
                     abs-s (Math/abs s)]
                 (if (zero? abs-s) (val/number 0.0)
                     (val/number (* abs-s (Math/ceil (/ n abs-s)))))))
             :arity [1 2])

(f/register! "ISO.CEILING" ;; alias per Excel
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     s (if (> (count args) 1) (f/num! (nth args 1)) 1.0)
                     abs-s (Math/abs s)]
                 (if (zero? abs-s) (val/number 0.0)
                     (val/number (* abs-s (Math/ceil (/ n abs-s)))))))
             :arity [1 2])

;; FLOOR.MATH(number, [significance=1], [mode=0])
;;   mode = 0: for negative numbers round toward -∞ (further from zero)
;;   mode != 0: for negative numbers round toward zero
(f/register! "FLOOR.MATH"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     s (if (> (count args) 1) (f/num! (nth args 1))
                           (if (neg? n) -1.0 1.0))
                     m (if (> (count args) 2) (f/num! (nth args 2)) 0.0)]
                 (if (zero? s) (val/number 0.0)
                     (let [abs-s (Math/abs s)]
                       (val/number
                        (cond
                          (zero? n) 0.0
                          (pos? n) (* abs-s (Math/floor (/ n abs-s)))
                          (zero? m) (* abs-s (Math/floor (/ n abs-s)))     ;; away from zero
                          :else     (* (- abs-s) (Math/floor (/ (- n) abs-s)))))))))
             :arity [1 3])

(f/register! "FLOOR.PRECISE"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     s (if (> (count args) 1) (f/num! (nth args 1)) 1.0)
                     abs-s (Math/abs s)]
                 (if (zero? abs-s) (val/number 0.0)
                     (val/number (* abs-s (Math/floor (/ n abs-s)))))))
             :arity [1 2])

(f/register! "MROUND"
  ;; MROUND(number, multiple) — nearest multiple (half away from zero).
  ;; Sign of number and multiple must match, else #NUM!.
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     m (f/num! (nth args 1))]
                 (cond
                   (zero? m) (val/number 0.0)
                   (and (neg? n) (pos? m)) (f/domain-error! :num)
                   (and (pos? n) (neg? m)) (f/domain-error! :num)
                   :else (val/number (* m (round-away-zero (/ n m)))))))
             :arity [2 2])

(f/register! "EVEN"
  ;; Round away from zero to nearest even integer.
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (cond
                   (zero? x) (val/number 0.0)
                   (pos? x)  (val/number (* 2.0 (Math/ceil (/ x 2.0))))
                   :else     (val/number (* 2.0 (Math/floor (/ x 2.0)))))))
             :arity [1 1])

(f/register! "ODD"
  ;; Round away from zero to nearest odd integer.
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (cond
                   (zero? x) (val/number 1.0)
                   (pos? x)  (val/number (inc (* 2.0 (Math/ceil (/ (dec x) 2.0)))))
                   :else     (val/number (dec (* 2.0 (Math/floor (/ (inc x) 2.0))))))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Factorial / combinatorics

(defn- fact ^double [^long n]
  (loop [i 2, acc 1.0]
    (if (> i n) acc (recur (inc i) (* acc (double i))))))

(f/register! "FACT"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     n (long x)]
                 (when (or (neg? n) (>= n 171)) (f/domain-error! :num))
                 (val/number (fact n))))
             :arity [1 1])

(f/register! "FACTDOUBLE"
  ;; n * (n-2) * (n-4) * ... down to 1 or 2.
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     n (long x)]
                 (cond
                   (< n -1) (f/domain-error! :num)
                   (<= n 0) (val/number 1.0)
                   :else
                   (val/number
                    (loop [i n, acc 1.0]
                      (if (<= i 1) acc (recur (- i 2) (* acc (double i)))))))))
             :arity [1 1])

(f/register! "COMBIN"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))
                     k (long (f/num! (nth args 1)))]
                 (when (or (neg? n) (neg? k) (> k n)) (f/domain-error! :num))
                 (let [k  (min k (- n k))
                       r  (loop [i 0, acc 1.0]
                            (if (= i k) acc
                                (recur (inc i) (/ (* acc (double (- n i))) (double (inc i))))))]
                   (val/number (f/check-num! r)))))
             :arity [2 2])

(f/register! "COMBINA"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))
                     k (long (f/num! (nth args 1)))]
                 (when (or (neg? n) (neg? k)) (f/domain-error! :num))
                 (let [n' (+ n k -1)
                       k' (min k (- n' k))
                       r  (loop [i 0, acc 1.0]
                            (if (= i k') acc
                                (recur (inc i) (/ (* acc (double (- n' i))) (double (inc i))))))]
                   (val/number (f/check-num! r)))))
             :arity [2 2])

(f/register! "PERMUT"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))
                     k (long (f/num! (nth args 1)))]
                 (when (or (neg? n) (neg? k) (> k n)) (f/domain-error! :num))
                 (val/number
                  (f/check-num!
                   (loop [i 0, acc 1.0]
                     (if (= i k) acc
                         (recur (inc i) (* acc (double (- n i))))))))))
             :arity [2 2])

(f/register! "PERMUTATIONA"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))
                     k (long (f/num! (nth args 1)))]
                 (when (or (neg? n) (neg? k)) (f/domain-error! :num))
                 (val/number (f/check-num! (Math/pow (double n) (double k))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; Variadic integer functions — GCD, LCM

(defn- gcd2 ^long [^long a ^long b]
  (if (zero? b) a (recur b (long (mod a b)))))

(f/register! "GCD"
             (fn [args]
               (let [ns (mapv (fn [v]
                                (let [x (f/num! v)
                                      n (long x)]
                                  (when (neg? n) (f/domain-error! :num))
                                  n))
                              (f/collect-scalars args))]
                 (val/number (double (reduce gcd2 0 ns)))))
             :arity [1 nil])

(f/register! "LCM"
             (fn [args]
               (let [ns (mapv (fn [v]
                                (let [x (f/num! v)
                                      n (long x)]
                                  (when (neg? n) (f/domain-error! :num))
                                  n))
                              (f/collect-scalars args))]
                 (val/number
                  (double
                   (reduce (fn [a b]
                             (if (or (zero? a) (zero? b)) 0
                                 (* (/ a (gcd2 a b)) b)))
                           1 ns)))))
             :arity [1 nil])

(f/register! "SQRTPI"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))]
                 (when (neg? x) (f/domain-error! :num))
                 (val/number (Math/sqrt (* x Math/PI)))))
             :arity [1 1])

(f/register! "MULTINOMIAL"
             (fn [args]
               (let [xs (mapv (fn [v]
                                (let [n (long (f/num! v))]
                                  (when (neg? n) (f/domain-error! :num))
                                  n))
                              (f/collect-scalars args))
                     tot (reduce + xs)
                     num (fact (long tot))
                     den (reduce * 1.0 (map #(fact (long %)) xs))]
                 (val/number (f/check-num! (/ num den)))))
             :arity [1 nil])

;; ---------------------------------------------------------------------------
;; Aggregate sums — SUM, SUMSQ, PRODUCT, and cross-array variants

(f/register! "SUM"
             (fn [args] (val/number (f/sum-numeric args)))
             :arity [1 nil])

(f/register! "SUMSQ"
             (fn [args]
               (let [acc (volatile! 0.0)]
                 (f/each-scalar
                  args
                  (fn [v in-area?]
                    (case (:t v)
                      :num   (vswap! acc #(+ (double %) (let [n (double (:v v))] (* n n))))
                      :bool  (when-not in-area?
                               (let [n (if (:v v) 1.0 0.0)]
                                 (vswap! acc #(+ (double %) (* n n)))))
                      :str   (when-not in-area? (f/domain-error! :value))
                      :err   (f/domain-error! (:v v))
                      nil)))
                 (val/number @acc)))
             :arity [1 nil])

(f/register! "PRODUCT"
             (fn [args]
               (let [acc (volatile! 1.0)
                     saw (volatile! false)]
                 (f/each-scalar
                  args
                  (fn [v in-area?]
                    (case (:t v)
                      :num   (do (vswap! acc #(* (double %) (double (:v v)))) (vreset! saw true))
                      :bool  (when-not in-area?
                               (vswap! acc #(* (double %) (if (:v v) 1.0 0.0)))
                               (vreset! saw true))
                      :str   (when-not in-area?
                               (if-let [n (p/parse-double (:v v))]
                                 (do (vswap! acc #(* (double %) (double n)))
                                     (vreset! saw true))
                                 (f/domain-error! :value)))
                      :err   (f/domain-error! (:v v))
                      nil)))
                 (val/number (if @saw @acc 0.0))))
             :arity [1 nil])

;; Area-pair helpers for SUMX2MY2 / SUMX2PY2 / SUMXMY2 / SUMPRODUCT.
;; Each expects two areas of identical shape (POI raises #N/A otherwise)
;; and iterates element-wise.

(defn- area-values [v]
  (cond
    (val/area? v) (:values v)
    (val/ref?  v) [[(-> v :resolved (or val/BLANK))]]
    :else         [[v]]))

(defn- same-shape? [a b]
  (and (= (count a) (count b))
       (every? true? (map (fn [r1 r2] (= (count r1) (count r2))) a b))))

(defn- pair-reduce
  "Element-wise reduce over two arg values of matching shape. `f` takes
  two doubles; POI coerces non-numeric cells by skipping pairs where
  either cell is non-numeric."
  ^double [op a b]
  (let [ra (area-values a)
        rb (area-values b)]
    (when-not (same-shape? ra rb) (f/domain-error! :na))
    (let [acc (volatile! 0.0)]
      (dotimes [i (count ra)]
        (let [row-a (nth ra i), row-b (nth rb i)]
          (dotimes [j (count row-a)]
            (let [xa (nth row-a j), xb (nth row-b j)]
              (cond
                (val/err? xa) (f/domain-error! (:v xa))
                (val/err? xb) (f/domain-error! (:v xb))
                (and (val/num? xa) (val/num? xb))
                (vswap! acc #(+ (double %) (double (op (:v xa) (:v xb))))))))))
      @acc)))

(f/register! "SUMX2MY2"
             (fn [args] (val/number (pair-reduce (fn [x y] (- (* x x) (* y y)))
                                                 (nth args 0) (nth args 1))))
             :arity [2 2] :array? true)

(f/register! "SUMX2PY2"
             (fn [args] (val/number (pair-reduce (fn [x y] (+ (* x x) (* y y)))
                                                 (nth args 0) (nth args 1))))
             :arity [2 2] :array? true)

(f/register! "SUMXMY2"
             (fn [args] (val/number (pair-reduce (fn [x y] (let [d (- x y)] (* d d)))
                                                 (nth args 0) (nth args 1))))
             :arity [2 2] :array? true)

(f/register! "SUMPRODUCT"
  ;; Element-wise product across N arrays of matching shape, summed.
             (fn [args]
               (let [areas (mapv area-values args)
                     a0    (first areas)
                     rows  (count a0)
                     cols  (count (first a0))]
                 (doseq [a (rest areas)]
                   (when-not (and (= rows (count a)) (= cols (count (first a))))
                     (f/domain-error! :value)))
                 (let [acc (volatile! 0.0)]
                   (dotimes [i rows]
                     (dotimes [j cols]
                       (let [prod (volatile! 1.0)
                             nonnum? (volatile! false)]
                         (doseq [a areas]
                           (let [cell (-> a (nth i) (nth j))]
                             (cond
                               (val/err? cell) (f/domain-error! (:v cell))
                               (val/num? cell) (vswap! prod #(* (double %) (double (:v cell))))
                               :else           (vreset! nonnum? true))))
                         (when-not @nonnum? (vswap! acc #(+ (double %) (double @prod)))))))
                   (val/number @acc))))
             :arity [1 nil] :array? true)

;; ---------------------------------------------------------------------------
;; Matrix — MMULT, MDETERM, MINVERSE, MUNIT, TRANSPOSE

(defn- matrix-from-area [v]
  (let [rows (area-values v)]
    (mapv (fn [row]
            (mapv (fn [c]
                    (let [n (val/to-num c)]
                      (if (val/num? n) (double (:v n))
                          (f/domain-error! :value))))
                  row))
          rows)))

(defn- area-from-matrix [m]
  {:t :area
   :values (mapv #(mapv val/number %) m)
   :r0 0 :c0 0
   :r1 (dec (count m))
   :c1 (dec (count (first m)))})

(defn- transpose-m [m]
  (let [rows (count m), cols (count (first m))]
    (mapv (fn [j] (mapv (fn [i] (get-in m [i j])) (range rows))) (range cols))))

(defn- matmul [a b]
  (let [ra (count a), ca (count (first a))
        rb (count b), cb (count (first b))]
    (when-not (= ca rb) (f/domain-error! :value))
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce + 0.0
                            (map (fn [k] (* (get-in a [i k]) (get-in b [k j])))
                                 (range ca))))
                  (range cb)))
          (range ra))))

(defn- det-m [m]
  (let [n (count m)]
    (when-not (= n (count (first m))) (f/domain-error! :value))
    ;; Gaussian elimination with partial pivoting.
    (loop [a (mapv (partial mapv double) m), sign 1.0, k 0, det 1.0]
      (if (= k n)
        (* sign det)
        (let [piv-row (reduce (fn [best i]
                                (if (> (Math/abs ^double (get-in a [i k]))
                                       (Math/abs ^double (get-in a [best k])))
                                  i best))
                              k (range (inc k) n))
              pv (get-in a [piv-row k])]
          (if (zero? pv)
            0.0
            (let [a (if (= piv-row k) a
                        (-> a (assoc k (nth a piv-row))
                            (assoc piv-row (nth a k))))
                  sign (if (= piv-row k) sign (- sign))
                  a (reduce (fn [a' i]
                              (let [f (/ (double (get-in a' [i k])) (double pv))]
                                (reduce (fn [a'' j]
                                          (assoc-in a'' [i j]
                                                    (- (double (get-in a'' [i j]))
                                                       (* f (double (get-in a'' [k j]))))))
                                        a' (range k n))))
                            a (range (inc k) n))]
              (recur a sign (inc k) (* det pv)))))))))

(defn- invert-m [m]
  (let [n (count m)]
    (when-not (= n (count (first m))) (f/domain-error! :value))
    (let [aug (mapv (fn [i row]
                      (vec (concat row
                                   (for [j (range n)]
                                     (if (= i j) 1.0 0.0)))))
                    (range n) m)
          ;; forward elimination with partial pivot
          aug (loop [a aug, k 0]
                (if (= k n) a
                    (let [piv-row (reduce (fn [best i]
                                            (if (> (Math/abs ^double (get-in a [i k]))
                                                   (Math/abs ^double (get-in a [best k])))
                                              i best))
                                          k (range (inc k) n))
                          pv (get-in a [piv-row k])
                          _  (when (zero? pv) (f/domain-error! :num))
                          a (if (= piv-row k) a
                                (-> a (assoc k (nth a piv-row))
                                    (assoc piv-row (nth a k))))
                          pv (get-in a [k k])
                          a (assoc a k (mapv #(/ % pv) (nth a k)))
                          a (reduce (fn [a' i]
                                      (if (= i k) a'
                                          (let [f (get-in a' [i k])]
                                            (assoc a' i
                                                   (mapv (fn [j]
                                                           (- (double (get-in a' [i j]))
                                                              (* f (double (get-in a' [k j])))))
                                                         (range (* 2 n)))))))
                                    a (range n))]
                      (recur a (inc k)))))]
      (mapv (fn [row] (vec (drop n row))) aug))))

(f/register! "MMULT"
             (fn [args]
               (area-from-matrix
                (matmul (matrix-from-area (nth args 0))
                        (matrix-from-area (nth args 1)))))
             :arity [2 2] :array? true)

(f/register! "MDETERM"
             (fn [args] (val/number (det-m (matrix-from-area (nth args 0)))))
             :arity [1 1] :array? true)

(f/register! "MINVERSE"
             (fn [args] (area-from-matrix (invert-m (matrix-from-area (nth args 0)))))
             :arity [1 1] :array? true)

(f/register! "MUNIT"
             (fn [args]
               (let [n (long (f/num! (nth args 0)))]
                 (when (<= n 0) (f/domain-error! :value))
                 (area-from-matrix
                  (mapv (fn [i]
                          (mapv (fn [j] (if (= i j) 1.0 0.0)) (range n)))
                        (range n)))))
             :arity [1 1] :array? true)

(f/register! "TRANSPOSE"
             (fn [args]
               (let [rows (area-values (nth args 0))]
                 {:t :area
                  :values (transpose-m (mapv vec rows))
                  :r0 0 :c0 0
                  :r1 (dec (count (first rows)))
                  :c1 (dec (count rows))}))
             :arity [1 1] :array? true)

;; ---------------------------------------------------------------------------
;; Random

(f/register! "RAND"
             (fn [_args] (val/number (rng/next-double)))
             :arity [0 0] :volatile? true)

(f/register! "RANDBETWEEN"
             (fn [args]
               (let [lo (Math/ceil (f/num! (nth args 0)))
                     hi (Math/floor (f/num! (nth args 1)))]
                 (when (> lo hi) (f/domain-error! :num))
                 (val/number (+ lo (Math/floor (* (rng/next-double) (+ 1.0 (- hi lo))))))))
             :arity [2 2] :volatile? true)

;; ---------------------------------------------------------------------------
;; Roman numerals

(def ^:private roman-pairs
  [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
   [100 "C"]  [90 "XC"]  [50 "L"]  [40 "XL"]
   [10 "X"]   [9 "IX"]   [5 "V"]   [4 "IV"]  [1 "I"]])

(defn- ->roman [^long n]
  (loop [n n, out (p/sb), pairs roman-pairs]
    (if (zero? n) (p/sb->str out)
        (let [[v s] (first pairs)]
          (if (>= n v)
            (recur (- n v) (p/sb-append! out s) pairs)
            (recur n out (rest pairs)))))))

;; Replacement rules copied verbatim from POI's Roman.java. Form N applies
;; rules 0..N (skipping rule 1 once N > 1, per POI's comment).
(def ^:private roman-replacements
  [;; form > 0
   ["XLV" "VL", "XCV" "VC", "CDL" "LD", "CML" "LM", "CMVC" "LMVL"]
   ;; form == 1 only
   ["CDXC" "LDXL", "CDVC" "LDVL", "CMXC" "LMXL", "XCIX" "VCIV", "XLIX" "VLIV"]
   ;; form > 1
   ["XLIX" "IL", "XCIX" "IC", "CDXC" "XD", "CDVC" "XDV", "CDIC" "XDIX",
    "LMVL" "XMV", "CMIC" "XMIX", "CMXC" "XM"]
   ;; form > 2
   ["XDV" "VD", "XDIX" "VDIV", "XMV" "VM", "XMIX" "VMIV"]
   ;; form == 4
   ["VDIV" "ID", "VMIV" "IM"]])

(defn- apply-replacements [s rules]
  (reduce (fn [acc [from to]] (str/replace acc from to))
          s
          (partition 2 rules)))

(defn- concise-roman [^String s ^long form]
  (if (zero? form)
    s
    (reduce (fn [acc i]
              (cond
                (> i form) (reduced acc)
                (> i 4)    (reduced acc)
                (and (= i 1) (> form 1)) acc   ;; POI skips rule 1 for form > 1
                :else      (apply-replacements acc (nth roman-replacements i))))
            s
            (range 0 5))))

(f/register! "ROMAN"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))]
                 (when (or (neg? n) (> n 3999)) (f/domain-error! :value))
                 (if (zero? n)
                   (val/string "")
                   (let [classic (->roman n)]
                     (if (<= (count args) 1)
                       (val/string classic)
                       (let [form (long (f/num! (nth args 1)))]
                         (when (or (neg? form) (> form 4)) (f/domain-error! :value))
                         (val/string (concise-roman classic form))))))))
             :arity [1 2])

(def ^:private roman-char->val
  {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn- roman-> [^String s]
  (let [s (str/upper-case s)
        n (count s)]
    (loop [i 0, acc 0]
      (if (>= i n) acc
          (let [cur (get roman-char->val (nth s i))
                nxt (get roman-char->val (get s (inc i)))]
            (when (nil? cur) (f/domain-error! :value))
            (if (and nxt (< cur nxt))
              (recur (+ i 2) (+ acc (- nxt cur)))
              (recur (inc i) (+ acc cur))))))))

(f/register! "ARABIC"
             ^{:scalar? true}
             (fn [args] (val/number (roman-> (f/str! (nth args 0)))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; SERIESSUM

(f/register! "SERIESSUM"
  ;; SERIESSUM(x, n, m, coefficients) = sum(coef[i] * x^(n + i*m))
             (fn [args]
               (let [x     (f/num! (nth args 0))
                     n     (f/num! (nth args 1))
                     m     (f/num! (nth args 2))
                     coefs (f/collect-scalars (vector (nth args 3)))
                     acc   (volatile! 0.0)]
                 (doseq [[i c] (map-indexed vector coefs)]
                   (let [cv (f/num! c)
                         power (+ n (* m (double i)))]
                     (vswap! acc #(+ (double %) (* cv (Math/pow x power))))))
                 (val/number (f/check-num! @acc))))
             :arity [4 4])

;; ---------------------------------------------------------------------------
;; AVEDEV — mean absolute deviation

(f/register! "AVEDEV"
             (fn [args]
               (let [xs (f/collect-finite-numerics args)]
                 (when (empty? xs) (f/domain-error! :num))
                 (let [n    (double (count xs))
                       mean (/ (reduce + 0.0 xs) n)
                       dev  (reduce (fn [a x] (+ a (Math/abs (- x mean)))) 0.0 xs)]
                   (val/number (/ dev n)))))
             :arity [1 nil])

;; ---------------------------------------------------------------------------
;; FREQUENCY — array-aware bin count

(f/register! "FREQUENCY"
             (fn [args]
               (let [data (f/collect-finite-numerics [(nth args 0)])
                     bins (f/collect-finite-numerics [(nth args 1)])
                     n    (count bins)
                     counts (long-array (inc n))]
                 (doseq [^double d data]
                   (let [idx (loop [i 0]
                               (cond
                                 (= i n) n
                                 (<= d (double (nth bins i))) i
                                 :else (recur (inc i))))]
                     (aset counts idx (inc (aget counts idx)))))
                 (area-from-matrix (mapv vector (map double (seq counts))))))
             :arity [2 2] :array? true)

;; ---------------------------------------------------------------------------
;; SUBTOTAL / AGGREGATE — dispatchers over the aggregate fns.

(def ^:private subtotal-dispatch
  {1 "AVERAGE", 2 "COUNT", 3 "COUNTA", 4 "MAX", 5 "MIN",
   6 "PRODUCT", 7 "STDEV", 8 "STDEVP", 9 "SUM", 10 "VAR", 11 "VARP",
   101 "AVERAGE", 102 "COUNT", 103 "COUNTA", 104 "MAX", 105 "MIN",
   106 "PRODUCT", 107 "STDEV", 108 "STDEVP", 109 "SUM", 110 "VAR", 111 "VARP"})

(f/register! "SUBTOTAL"
             (fn [args]
               (let [code (long (f/num! (nth args 0)))
                     fname (or (subtotal-dispatch code) (f/domain-error! :value))]
                 (f/call fname (subvec (vec args) 1))))
             :arity [2 nil])

(def ^:private aggregate-dispatch
  {1 "AVERAGE", 2 "COUNT", 3 "COUNTA", 4 "MAX", 5 "MIN",
   6 "PRODUCT", 7 "STDEV.S", 8 "STDEV.P", 9 "SUM",
   10 "VAR.S", 11 "VAR.P", 12 "MEDIAN", 13 "MODE.SNGL",
   14 "LARGE", 15 "SMALL", 16 "PERCENTILE.INC", 17 "QUARTILE.INC",
   18 "PERCENTILE.EXC", 19 "QUARTILE.EXC"})

(f/register! "AGGREGATE"
  ;; Full AGGREGATE has two options controls; we ignore `options` for now
  ;; (which means ignore-error / ignore-hidden). POI does the same for
  ;; most cases.
             (fn [args]
               (let [code (long (f/num! (nth args 0)))
                     fname (or (aggregate-dispatch code) (f/domain-error! :value))
          ;; arg 1 is options; the remaining are the data (plus optional k for
          ;; LARGE/SMALL/PERCENTILE/QUARTILE which expect k as the last arg).
                     data (subvec (vec args) 2)]
                 (f/call fname data)))
             :arity [3 nil])
