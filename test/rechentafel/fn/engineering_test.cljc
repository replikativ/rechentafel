(ns rechentafel.fn.engineering-test
  "Sanity tests for BESSEL and CONVERT. Known values come from IronCalc's
  bessel test suite (derived from Arb/Nemo.jl)."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.engineering]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- close? [expected actual]
  (and (v/num? actual)
       (let [a    (double expected)
             b    (double (:v actual))
             diff (Math/abs (- a b))]
         (or (== a b)
             (< diff 1e-9)
             (< (/ diff (Math/sqrt (+ (* a a) (* b b)))) 1e-6)))))

(deftest besselj-known-values
  (is (close? 1.0 (f/call "BESSELJ" [(n 0.0) (n 0)])))
  (is (close? 0.7651976865579666  (f/call "BESSELJ" [(n 1.0) (n 0)])))
  (is (close? 0.002507683297243813 (f/call "BESSELJ" [(n 2.4) (n 0)])))
  (is (close? 0.4400505857449335  (f/call "BESSELJ" [(n 1.0) (n 1)])))
  (is (close? 0.129211228759725   (f/call "BESSELJ" [(n 30.0) (n 3)])))
  (testing "J_n(0) = 0 for n >= 1"
    (is (close? 0.0 (f/call "BESSELJ" [(n 0.0) (n 7)])))))

(deftest besselj-negative-order-errors
  (is (= :num (:v (f/call "BESSELJ" [(n 1.0) (n -1)])))))

(deftest bessely-known-values
  (is (close? 0.08825696421567692  (f/call "BESSELY" [(n 1.0) (n 0)])))
  (is (close? 0.5104147486657438   (f/call "BESSELY" [(n 2.4) (n 0)]))))

(deftest besseli-known-values
  (is (close? 1.0634833707413236    (f/call "BESSELI" [(n 0.5) (n 0)])))
  (is (close? 0.2578943053908963    (f/call "BESSELI" [(n 0.5) (n 1)])))
  (is (close? 0.002645111968990286  (f/call "BESSELI" [(n 0.5) (n 3)])))
  (testing "I_n(0) = 0 for n >= 1"
    (is (close? 0.0 (f/call "BESSELI" [(n 0.0) (n 7)])))))

(deftest besselk-known-values
  (is (close? 0.9244190712276659  (f/call "BESSELK" [(n 0.5) (n 0)])))
  (is (close? 1.656441120003301   (f/call "BESSELK" [(n 0.5) (n 1)])))
  (is (close? 62.05790952993026   (f/call "BESSELK" [(n 0.5) (n 3)]))))

(deftest convert-linear
  (is (close? 0.3048       (f/call "CONVERT" [(n 1.0) (s "ft") (s "m")])))
  (is (close? 0.45359237   (f/call "CONVERT" [(n 1.0) (s "lbm") (s "kg")])))
  (is (close? 7200.0       (f/call "CONVERT" [(n 2.0) (s "hr") (s "sec")])))
  (is (close? 1.0          (f/call "CONVERT" [(n 1000.0) (s "ml") (s "l")])))
  (is (close? 1.0          (f/call "CONVERT" [(n 1e6) (s "ug") (s "g")]))))

(deftest convert-area-squares-prefix
  (testing "1 km² = 1,000,000 m²"
    (is (close? 1e6 (f/call "CONVERT" [(n 1.0) (s "km2") (s "m2")])))))

(deftest convert-temperature
  (is (close? 32.0   (f/call "CONVERT" [(n 0.0)    (s "C") (s "F")])))
  (is (close? 0.0    (f/call "CONVERT" [(n 32.0)   (s "F") (s "C")])))
  (is (close? 0.0    (f/call "CONVERT" [(n 273.15) (s "K") (s "C")]))))

(deftest convert-mismatched-kinds
  (testing "mass to distance returns #N/A"
    (is (= :na (:v (f/call "CONVERT" [(n 1.0) (s "g") (s "m")]))))))

(deftest convert-unknown-units
  (is (= :na (:v (f/call "CONVERT" [(n 1.0) (s "foo") (s "bar")])))))

;; ---------------------------------------------------------------------------
;; Bit operations (Excel 2013+). Semantics pulled from IronCalc's
;; src/functions/engineering/bit.rs:
;;   - inputs must be integers in [0, 2^48-1]
;;   - shift count in [-53, 53]; negative flips direction
;;   - overflow of the result above 2^48 returns #NUM!

(deftest bit-and-or-xor
  (is (= {:t :num :v 8.0}   (f/call "BITAND" [(n 12) (n 10)])))  ;; 1100 & 1010
  (is (= {:t :num :v 14.0}  (f/call "BITOR"  [(n 12) (n 10)])))  ;; 1100 | 1010
  (is (= {:t :num :v 6.0}   (f/call "BITXOR" [(n 12) (n 10)])))  ;; 1100 ^ 1010
  (is (= {:t :num :v 0.0}   (f/call "BITAND" [(n 0)  (n 255)])))
  (is (= {:t :num :v 255.0} (f/call "BITOR"  [(n 0)  (n 255)]))))

(deftest bit-shift
  (is (= {:t :num :v 32.0}  (f/call "BITLSHIFT" [(n 1)  (n 5)])))   ;; 1 << 5
  (is (= {:t :num :v 1.0}   (f/call "BITRSHIFT" [(n 32) (n 5)])))   ;; 32 >> 5
  (testing "negative shift flips direction"
    (is (= {:t :num :v 1.0}  (f/call "BITLSHIFT" [(n 32) (n -5)])))
    (is (= {:t :num :v 32.0} (f/call "BITRSHIFT" [(n 1)  (n -5)]))))
  (testing "shift count out of [-53, 53] → #NUM!"
    (is (= :num (:v (f/call "BITLSHIFT" [(n 1) (n 54)]))))
    (is (= :num (:v (f/call "BITRSHIFT" [(n 1) (n -54)]))))))

(deftest bit-domain-errors
  (testing "negative arg → #NUM!"
    (is (= :num (:v (f/call "BITAND" [(n -1) (n 1)]))))
    (is (= :num (:v (f/call "BITOR"  [(n 1)  (n -1)]))))
    (is (= :num (:v (f/call "BITXOR" [(n -1) (n -1)])))))
  (testing "result ≥ 2^48 → #NUM!"
    (is (= :num (:v (f/call "BITLSHIFT" [(n 1) (n 48)]))))))
