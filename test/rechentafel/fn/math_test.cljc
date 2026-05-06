(ns rechentafel.fn.math-test
  "Sanity tests for the math function module. Goal is not to re-verify
  every numeric edge case POI already tests — those are tracked via
  parity tests elsewhere — but to exercise every registered fn at least
  once so a breakage in registration, arity, or coercion fails loud."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.math]))

(defn- n= [expected actual]
  (and (v/num? actual)
       (let [diff (Math/abs (- (double expected) (double (:v actual))))]
         (< diff 1e-9))))

(defn- n [x] (v/number x))
(defn- area [rows]
  {:t :area
   :values (mapv #(mapv v/number %) rows)
   :r0 0 :c0 0 :r1 (dec (count rows)) :c1 (dec (count (first rows)))})

(deftest unary-math
  (is (n= 3.0 (f/call "ABS"  [(n -3)])))
  (is (n= 1.0 (f/call "SIGN" [(n 7)])))
  (is (n= 0.0 (f/call "SIGN" [(n 0)])))
  (is (n= 4.0 (f/call "SQRT" [(n 16)])))
  (is (n= 1.0 (f/call "EXP"  [(n 0)])))
  (is (n= 0.0 (f/call "LN"   [(n 1)])))
  (is (n= 2.0 (f/call "LOG10" [(n 100)])))
  (is (n= 3.0 (f/call "INT"  [(n 3.7)])))
  (is (n= -4.0 (f/call "INT" [(n -3.5)])))   ;; floor semantics
  (is (n= 3.0 (f/call "TRUNC" [(n 3.7)])))
  (is (n= -3.0 (f/call "TRUNC" [(n -3.7)])))
  (is (n= 4.0 (f/call "EVEN" [(n 3)])))
  (is (n= 5.0 (f/call "ODD"  [(n 4)]))))

(deftest trig
  (is (n= 0.0 (f/call "SIN" [(n 0)])))
  (is (n= 1.0 (f/call "COS" [(n 0)])))
  (is (n= 0.0 (f/call "TAN" [(n 0)])))
  (is (n= 180.0 (f/call "DEGREES" [(n Math/PI)])))
  (is (n= Math/PI (f/call "RADIANS" [(n 180)])))
  (is (n= Math/PI (f/call "PI" []))))

(deftest binary-math
  (is (n= 1024.0 (f/call "POWER" [(n 2) (n 10)])))
  (is (n= 3.0 (f/call "LOG" [(n 1000) (n 10)])))
  (is (n= 1.0 (f/call "MOD" [(n -5) (n 3)])))  ;; sign-of-divisor
  (is (n= 2.0 (f/call "QUOTIENT" [(n 7) (n 3)])))
  (is (= v/ERR-DIV0 (f/call "MOD" [(n 5) (n 0)]))))

(deftest rounding
  (is (n= 3.14 (f/call "ROUND" [(n 3.14159) (n 2)])))
  (is (n= 4.0  (f/call "ROUND" [(n 3.5) (n 0)])))        ;; half away from 0
  (is (n= -4.0 (f/call "ROUND" [(n -3.5) (n 0)])))
  (is (n= 4.0  (f/call "ROUNDUP"  [(n 3.2) (n 0)])))
  (is (n= 3.0  (f/call "ROUNDDOWN" [(n 3.9) (n 0)])))
  (is (n= 3.0  (f/call "CEILING" [(n 2.5) (n 1)])))
  (is (n= 2.0  (f/call "FLOOR"   [(n 2.5) (n 1)])))
  (is (n= 3.0  (f/call "CEILING.PRECISE" [(n 2.5)])))
  (is (n= 2.0  (f/call "FLOOR.PRECISE"   [(n 2.5)])))
  (is (n= -2.0 (f/call "CEILING.MATH" [(n -2.5)])))     ;; toward 0 default
  (is (n= -3.0 (f/call "FLOOR.MATH"   [(n -2.5)])))
  (is (n= 10.0 (f/call "MROUND" [(n 12) (n 5)])))
  (is (= v/ERR-NUM (f/call "MROUND" [(n -5) (n 3)]))))

(deftest combinatorics
  (is (n= 120.0 (f/call "FACT" [(n 5)])))
  (is (n= 15.0  (f/call "FACTDOUBLE" [(n 5)])))   ;; 5*3*1
  (is (n= 10.0  (f/call "COMBIN" [(n 5) (n 2)])))
  (is (n= 15.0  (f/call "COMBINA" [(n 5) (n 2)])))  ;; C(6,2)
  (is (n= 20.0  (f/call "PERMUT" [(n 5) (n 2)])))
  (is (n= 25.0  (f/call "PERMUTATIONA" [(n 5) (n 2)])))
  (is (n= 6.0   (f/call "GCD" [(n 12) (n 18)])))
  (is (n= 12.0  (f/call "LCM" [(n 4) (n 6)])))
  (is (n= 1260.0 (f/call "MULTINOMIAL" [(n 2) (n 3) (n 4)])))
  (is (n= (Math/sqrt (* 9 Math/PI))
          (f/call "SQRTPI" [(n 9)]))))

(deftest aggregate-sums
  (is (n= 6.0  (f/call "SUM"     [(n 1) (n 2) (n 3)])))
  (is (n= 14.0 (f/call "SUMSQ"   [(n 1) (n 2) (n 3)])))
  (is (n= 24.0 (f/call "PRODUCT" [(n 2) (n 3) (n 4)])))
  (is (n= 70.0 (f/call "SUMPRODUCT" [(area [[1 2] [3 4]]) (area [[5 6] [7 8]])])))
  (is (n= -144.0 (f/call "SUMX2MY2" [(area [[1 2] [3 4]]) (area [[5 6] [7 8]])])))
  (is (n= 204.0 (f/call "SUMX2PY2" [(area [[1 2] [3 4]]) (area [[5 6] [7 8]])])))
  (is (n= 64.0  (f/call "SUMXMY2" [(area [[1 2] [3 4]]) (area [[5 6] [7 8]])])))
  (is (n= 15.0 (f/call "SERIESSUM" [(n 2) (n 0) (n 1) (area [[1 1 1 1]])]))))

(deftest matrix-ops
  (let [A (area [[1 2] [3 4]])
        B (area [[5 6] [7 8]])
        I (area [[1 0] [0 1]])
        M (f/call "MMULT" [A B])]
    (is (v/area? M))
    (is (= 19.0 (:v (get-in M [:values 0 0]))))
    (is (= 50.0 (:v (get-in M [:values 1 1])))))
  (is (n= -2.0 (f/call "MDETERM" [(area [[1 2] [3 4]])])))
  (let [inv (f/call "MINVERSE" [(area [[1 2] [3 4]])])]
    (is (v/area? inv))
    (is (< (Math/abs (- -2.0 (:v (get-in inv [:values 0 0])))) 1e-9)))
  (let [U (f/call "MUNIT" [(n 3)])]
    (is (v/area? U))
    (is (= 1.0 (:v (get-in U [:values 1 1]))))
    (is (= 0.0 (:v (get-in U [:values 0 1])))))
  (let [T (f/call "TRANSPOSE" [(area [[1 2 3]])])]
    (is (v/area? T))
    (is (= 3 (count (:values T))))
    (is (= 1 (count (first (:values T)))))))

(deftest roman-numerals
  (is (= (v/string "MCMXCIV") (f/call "ROMAN" [(n 1994)])))
  (is (n= 1994.0 (f/call "ARABIC" [(v/string "MCMXCIV")])))
  (is (= v/ERR-VALUE (f/call "ROMAN" [(n -1)])))
  (is (= v/ERR-VALUE (f/call "ROMAN" [(n 4000)]))))

(deftest frequency-and-avedev
  (is (n= 1.2 (f/call "AVEDEV" [(n 1) (n 2) (n 3) (n 4) (n 5)])))
  (let [out (f/call "FREQUENCY"
                    [(area [[10 20 30 40 50]])
                     (area [[20 40]])])]
    (is (v/area? out))
    (is (= 3 (count (:values out))))
    (is (= 2.0 (:v (get-in out [:values 0 0]))))
    (is (= 2.0 (:v (get-in out [:values 1 0]))))
    (is (= 1.0 (:v (get-in out [:values 2 0]))))))

(deftest subtotal-dispatch
  (is (n= 6.0 (f/call "SUBTOTAL" [(n 9) (n 1) (n 2) (n 3)])))  ;; SUM
  (is (= v/ERR-VALUE (f/call "SUBTOTAL" [(n 999) (n 1)]))))   ;; bad code

(deftest reciprocal-trig
  ;; Excel 2013+ reciprocal-trig family. Vectors cross-checked against
  ;; IronCalc function definitions and direct math.
  (is (n= (/ 1.0 (Math/tan 1.0))  (f/call "COT"  [(n 1.0)])))
  (is (n= (/ 1.0 (Math/tanh 1.0)) (f/call "COTH" [(n 1.0)])))
  (is (n= (/ 1.0 (Math/cos 1.0))  (f/call "SEC"  [(n 1.0)])))
  (is (n= (/ 1.0 (Math/cosh 1.0)) (f/call "SECH" [(n 1.0)])))
  (is (n= (/ 1.0 (Math/sin 1.0))  (f/call "CSC"  [(n 1.0)])))
  (is (n= (/ 1.0 (Math/sinh 1.0)) (f/call "CSCH" [(n 1.0)])))
  (testing "poles return #DIV/0! (Excel convention)"
    (is (= v/ERR-DIV0 (f/call "COT"  [(n 0.0)])))
    (is (= v/ERR-DIV0 (f/call "CSC"  [(n 0.0)])))
    (is (= v/ERR-DIV0 (f/call "COTH" [(n 0.0)])))
    (is (= v/ERR-DIV0 (f/call "CSCH" [(n 0.0)])))))

(deftest inverse-cotangent
  ;; Excel's ACOT principal branch is (0, π).
  (is (n= (/ Math/PI 2.0)         (f/call "ACOT" [(n 0.0)])))       ;; π/2
  (is (n= (/ Math/PI 4.0)         (f/call "ACOT" [(n 1.0)])))       ;; π/4
  (is (n= (* 3.0 (/ Math/PI 4.0)) (f/call "ACOT" [(n -1.0)])))      ;; 3π/4
  (testing "ACOTH needs |x| > 1"
    ;; ACOTH(x) = 0.5 ln((x+1)/(x-1))
    (is (n= 0.34657359027997264 (f/call "ACOTH" [(n 3.0)])))  ;; 0.5 ln 2
    (is (n= 0.5493061443340548  (f/call "ACOTH" [(n 2.0)])))  ;; 0.5 ln 3
    (is (= v/ERR-NUM (f/call "ACOTH" [(n 1.0)])))
    (is (= v/ERR-NUM (f/call "ACOTH" [(n 0.5)])))))

(deftest gauss-phi
  ;; PHI(x) = (1/√(2π)) e^{-x²/2}; GAUSS(x) = Φ(x) − 0.5.
  (is (n= 0.3989422804014327 (f/call "PHI" [(n 0.0)])))  ;; 1/√(2π)
  (is (n= 0.24197072451914337 (f/call "PHI" [(n 1.0)])))
  (is (n= 0.05399096651318806 (f/call "PHI" [(n 2.0)])))
  (is (n= 0.0 (f/call "GAUSS" [(n 0.0)])))
  ;; GAUSS uses A&S 7.1.26 (≈1e-7 accuracy), loosen tolerance.
  (let [g1 (f/call "GAUSS" [(n 1.0)])
        g2 (f/call "GAUSS" [(n 2.0)])]
    (is (v/num? g1))
    (is (< (Math/abs (- (:v g1) 0.3413447460685429)) 1e-6))
    (is (< (Math/abs (- (:v g2) 0.4772498680518208)) 1e-6))))

(deftest error-semantics
  (is (= v/ERR-NUM   (f/call "SQRT"  [(n -1)])))
  (is (= v/ERR-NUM   (f/call "LN"    [(n 0)])))
  (is (= v/ERR-NUM   (f/call "ACOS"  [(n 2)])))
  (is (= v/ERR-DIV0  (f/call "ATAN2" [(n 0) (n 0)])))
  ;; first-error short-circuits
  (is (= v/ERR-VALUE (f/call "ABS" [v/ERR-VALUE])))
  (is (= v/ERR-DIV0  (f/call "SUM" [(n 1) v/ERR-DIV0 (n 2)])))
  ;; arity mismatch
  (is (= v/ERR-VALUE (f/call "ABS" [])))
  (is (= v/ERR-VALUE (f/call "PI" [(n 1)])))
  ;; unknown fn
  (is (= v/ERR-NAME  (f/call "NOPE" [(n 1)]))))
