(ns rechentafel.fn.engineering
  "Engineering functions (POI category: engineering — 39 fns).

  Covers base conversions (BIN/OCT/DEC/HEX in all 12 directions), the
  complex-number family (IM*), DELTA/GESTEP, ERF/ERFC, and Bessel
  stubs. POI marks many of these NotImplemented; we fill them in
  where the math is tractable (base conversions, complex arithmetic,
  ERF/ERFC via series) and leave BESSEL*/CONVERT as #N/A stubs."
  (:require [clojure.string :as str]
            [rechentafel.platform :as p]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; Cross-platform numeric edge values.
(def ^:private NaN  ##NaN)
(def ^:private +Inf ##Inf)

;; ---------------------------------------------------------------------------
;; Base conversions
;;
;; Excel's BIN/OCT/HEX → DEC functions take strings up to 10 characters
;; and treat the high bit as a sign (two's-complement). E.g.
;; BIN2DEC(\"1111111111\") = -1, BIN2DEC(\"1000000000\") = -512.
;;
;; DEC2* take a number and an optional minimum-length argument, padding
;; with zeros. Negative numbers use two's-complement with 10 chars.

(defn- parse-in-base [^String s ^long base ^long max-bits]
  (let [s (str/upper-case (str s))
        n (count s)]
    (when (> n 10) (f/domain-error! :num))
    (let [v #?(:clj (try (Long/parseLong s base)
                         (catch Throwable _
                           (f/domain-error! :num)))
               :cljs (let [x (js/parseInt s base)]
                       (if (js/isNaN x) (f/domain-error! :num) x)))]
      (cond
        (and (= n 10) (>= v (bit-shift-left 1 (dec max-bits))))
        (- v (bit-shift-left 1 max-bits))
        :else v))))

(defn- to-excel-digits [^long n ^long base ^long digits]
  (let [s (if (neg? n)
            ;; two's-complement in 10 digits
            (let [mask-bits (case base 2 10, 8 30, 16 40, 10 40)
                  modulus (bit-shift-left 1 mask-bits)
                  v (+ n modulus)
                  raw #?(:clj (Long/toString v base)
                         :cljs (.toString v base))]
              (str/upper-case raw))
            (let [raw #?(:clj (Long/toString n base)
                         :cljs (.toString n base))
                  raw (str/upper-case raw)]
              (if (pos? digits)
                (let [pad (- digits (count raw))]
                  (if (pos? pad) (str (str/join (repeat pad "0")) raw) raw))
                raw)))]
    s))

(defn- bindec-like [base max-bits]
  (with-meta
    (fn [args]
      (val/number (double (parse-in-base (f/str! (nth args 0)) base max-bits))))
    {:scalar? true}))

(f/register! "BIN2DEC" (bindec-like 2 10) :arity [1 1])
(f/register! "OCT2DEC" (bindec-like 8 30) :arity [1 1])
(f/register! "HEX2DEC" (bindec-like 16 40) :arity [1 1])

(defn- dec-to-base [base]
  (with-meta
    (fn [args]
      (let [n (long (f/num! (nth args 0)))
            d (if (> (count args) 1) (long (f/num! (nth args 1))) 0)
            bit-limit (case base 2 9, 8 29, 16 39)
            lo (- (bit-shift-left 1 bit-limit))
            hi (dec (bit-shift-left 1 bit-limit))]
        (when (or (< n lo) (> n hi)) (f/domain-error! :num))
        (val/string (to-excel-digits n base d))))
    {:scalar? true}))

(f/register! "DEC2BIN" (dec-to-base 2)  :arity [1 2])
(f/register! "DEC2OCT" (dec-to-base 8)  :arity [1 2])
(f/register! "DEC2HEX" (dec-to-base 16) :arity [1 2])

(defn- convert-base [from-base to-base max-bits]
  (with-meta
    (fn [args]
      (let [n (parse-in-base (f/str! (nth args 0)) from-base max-bits)
            d (if (> (count args) 1) (long (f/num! (nth args 1))) 0)]
        (val/string (to-excel-digits n to-base d))))
    {:scalar? true}))

(f/register! "BIN2OCT" (convert-base 2 8 10)  :arity [1 2])
(f/register! "BIN2HEX" (convert-base 2 16 10) :arity [1 2])
(f/register! "OCT2BIN" (convert-base 8 2 30)  :arity [1 2])
(f/register! "OCT2HEX" (convert-base 8 16 30) :arity [1 2])
(f/register! "HEX2BIN" (convert-base 16 2 40) :arity [1 2])
(f/register! "HEX2OCT" (convert-base 16 8 40) :arity [1 2])

;; ---------------------------------------------------------------------------
;; Bit operations (Excel 2013+). Inputs must be non-negative integers
;; < 2^48 (Excel's documented limit); shift counts must be in [-53, 53].
;; All results are non-negative integers.

(defn- bit-int! [x]
  (let [n (long (f/num! x))]
    (when (or (neg? n) (>= n (bit-shift-left 1 48)))
      (f/domain-error! :num))
    n))

(f/register! "BITAND"
             ^{:scalar? true}
             (fn [args] (val/number (double (bit-and (bit-int! (nth args 0))
                                                     (bit-int! (nth args 1))))))
             :arity [2 2])
(f/register! "BITOR"
             ^{:scalar? true}
             (fn [args] (val/number (double (bit-or (bit-int! (nth args 0))
                                                    (bit-int! (nth args 1))))))
             :arity [2 2])
(f/register! "BITXOR"
             ^{:scalar? true}
             (fn [args] (val/number (double (bit-xor (bit-int! (nth args 0))
                                                     (bit-int! (nth args 1))))))
             :arity [2 2])

(defn- bit-shift-fn [dir]
  ;; dir = :left or :right. A negative shift count flips direction.
  (with-meta
    (fn [args]
      (let [n (bit-int! (nth args 0))
            k (long (f/num! (nth args 1)))]
        (when (> (Math/abs k) 53) (f/domain-error! :num))
        (let [[d amt] (if (neg? k)
                        [(if (= dir :left) :right :left) (- k)]
                        [dir k])
              r (if (= d :left)
                  (bit-shift-left n amt)
                  (bit-shift-right n amt))]
          (when (>= r (bit-shift-left 1 48)) (f/domain-error! :num))
          (val/number (double r)))))
    {:scalar? true}))

(f/register! "BITLSHIFT" (bit-shift-fn :left)  :arity [2 2])
(f/register! "BITRSHIFT" (bit-shift-fn :right) :arity [2 2])

;; ---------------------------------------------------------------------------
;; DELTA / GESTEP

(f/register! "DELTA"
             ^{:scalar? true}
             (fn [args]
               (let [a (f/num! (nth args 0))
                     b (if (> (count args) 1) (f/num! (nth args 1)) 0.0)]
                 (val/number (if (== a b) 1.0 0.0))))
             :arity [1 2])

(f/register! "GESTEP"
             ^{:scalar? true}
             (fn [args]
               (let [n (f/num! (nth args 0))
                     s (if (> (count args) 1) (f/num! (nth args 1)) 0.0)]
                 (val/number (if (>= n s) 1.0 0.0))))
             :arity [1 2])

;; ---------------------------------------------------------------------------
;; ERF / ERFC — Abramowitz & Stegun 7.1.26 approximation
;; (Maximum error ~1.5e-7.)

(defn- erf-approx ^double [^double x]
  (let [sign (if (neg? x) -1.0 1.0)
        ax   (Math/abs x)
        t (/ 1.0 (+ 1.0 (* 0.3275911 ax)))
        y (- 1.0
             (* (+ (* 0.254829592 t)
                   (* -0.284496736 (* t t))
                   (* 1.421413741 (* t t t))
                   (* -1.453152027 (* t t t t))
                   (* 1.061405429 (* t t t t t)))
                (Math/exp (- (* ax ax)))))]
    (* sign y)))

(f/register! "ERF"
             ^{:scalar? true}
             (fn [args]
               (let [a (f/num! (nth args 0))
                     b (if (> (count args) 1) (f/num! (nth args 1)) nil)]
                 (val/number (if b (- (erf-approx b) (erf-approx a))
                                 (erf-approx a)))))
             :arity [1 2])

(f/register! "ERFC"
             ^{:scalar? true}
             (fn [args]
               (val/number (- 1.0 (erf-approx (f/num! (nth args 0))))))
             :arity [1 1])

(f/register! "ERF.PRECISE"
             ^{:scalar? true}
             (fn [args] (val/number (erf-approx (f/num! (nth args 0)))))
             :arity [1 1])

(f/register! "ERFC.PRECISE"
             ^{:scalar? true}
             (fn [args] (val/number (- 1.0 (erf-approx (f/num! (nth args 0))))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Complex numbers — represented as strings "a+bi" or "a+bj".

(def ^:private imag-suffix-re #"(?i)([ij])$")

(def ^:private complex-re
  #"(?i)^([+-]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][+-]?\d+)?)?([+-](?:\d+(?:\.\d+)?|\.\d+)(?:[eE][+-]?\d+)?)?([ij])?$")

(defn- parse-complex [^String s]
  (if-let [[_ re-str im-str suffix] (re-matches complex-re s)]
    (let [re-str (when (and re-str (not= re-str "")) re-str)
          im-str (when (and im-str (not= im-str "")) im-str)
          suffix (when (and suffix (not= suffix "")) suffix)]
      (cond
        ;; pure real: no suffix, only re
        (and (nil? suffix) (nil? im-str))
        [(p/parse-double re-str) 0.0]

        ;; pure imaginary: no re_str, only suffix (possibly with sign "i", "-i")
        (and suffix (nil? im-str))
        (let [coef (cond
                     (nil? re-str) 1.0
                     (= re-str "+") 1.0
                     (= re-str "-") -1.0
                     :else (p/parse-double re-str))]
          [0.0 coef])

        :else
        [(if re-str (p/parse-double re-str) 0.0)
         (if im-str (p/parse-double im-str) 0.0)]))
    (f/domain-error! :num)))

(defn- fmt-num [^double d]
  (let [i (long d)]
    (if (== (double i) d) (str i) (str d))))

(defn- format-complex
  ([^double re ^double im] (format-complex re im "i"))
  ([^double re ^double im unit]
   (cond
     (zero? im) (fmt-num re)
     (zero? re) (cond
                  (== im 1.0)  unit
                  (== im -1.0) (str "-" unit)
                  :else        (str (fmt-num im) unit))
     :else
     (let [im-part (cond
                     (== im 1.0)  (str "+" unit)
                     (== im -1.0) (str "-" unit)
                     (pos? im)    (str "+" (fmt-num im) unit)
                     :else        (str (fmt-num im) unit))]
       (str (fmt-num re) im-part)))))

(defn- complex-arg [v]
  (cond
    (val/num? v) [(double (:v v)) 0.0]
    (val/str? v) (parse-complex (:v v))
    :else (f/domain-error! :num)))

(f/register! "COMPLEX"
             ^{:scalar? true}
             (fn [args]
               (let [re (f/num! (nth args 0))
                     im (f/num! (nth args 1))
                     u  (if (> (count args) 2) (f/str! (nth args 2)) "i")]
                 (when-not (or (= u "i") (= u "j")) (f/domain-error! :value))
                 (val/string (format-complex re im u))))
             :arity [2 3])

(defn- reg-im [name arity impl]
  (f/register! name
               (with-meta (fn [args] (impl args)) {:scalar? true})
               :arity arity))

(reg-im "IMREAL"    [1 1] (fn [args] (val/number (first (complex-arg (nth args 0))))))
(reg-im "IMAGINARY" [1 1] (fn [args] (val/number (second (complex-arg (nth args 0))))))

(reg-im "IMABS" [1 1]
        (fn [args]
          (let [[r i] (complex-arg (nth args 0))]
            (val/number (Math/hypot r i)))))

(reg-im "IMARGUMENT" [1 1]
        (fn [args]
          (let [[r i] (complex-arg (nth args 0))]
            (when (and (zero? r) (zero? i)) (f/domain-error! :div0))
            (val/number (Math/atan2 i r)))))

(reg-im "IMCONJUGATE" [1 1]
        (fn [args]
          (let [[r i] (complex-arg (nth args 0))]
            (val/string (format-complex r (- i))))))

;; IMSUM/IMPRODUCT are aggregates (variadic over complex args). Register
;; directly (not via reg-im) so they are NOT marked :scalar?.
(f/register! "IMSUM"
             (fn [args]
               (let [acc (volatile! [0.0 0.0])]
                 (doseq [a args]
                   (let [[r i] (complex-arg a)]
                     (vswap! acc (fn [[ar ai]] [(+ ar r) (+ ai i)]))))
                 (let [[r i] @acc]
                   (val/string (format-complex r i)))))
             :arity [1 nil])

(reg-im "IMSUB" [2 2]
        (fn [args]
          (let [[ar ai] (complex-arg (nth args 0))
                [br bi] (complex-arg (nth args 1))]
            (val/string (format-complex (- ar br) (- ai bi))))))

(defn- cmul [[ar ai] [br bi]]
  [(- (* ar br) (* ai bi)) (+ (* ar bi) (* ai br))])

(f/register! "IMPRODUCT"
             (fn [args]
               (let [acc (volatile! [1.0 0.0])]
                 (doseq [a args]
                   (vswap! acc cmul (complex-arg a)))
                 (let [[r i] @acc]
                   (val/string (format-complex r i)))))
             :arity [1 nil])

(defn- cdiv [[ar ai] [br bi]]
  (let [d (+ (* br br) (* bi bi))]
    (when (zero? d) (f/domain-error! :div0))
    [(/ (+ (* ar br) (* ai bi)) d)
     (/ (- (* ai br) (* ar bi)) d)]))

(reg-im "IMDIV" [2 2]
        (fn [args]
          (let [[r i] (cdiv (complex-arg (nth args 0))
                            (complex-arg (nth args 1)))]
            (val/string (format-complex r i)))))

(defn- cexp [[ar ai]]
  (let [m (Math/exp ar)]
    [(* m (Math/cos ai)) (* m (Math/sin ai))]))

(reg-im "IMEXP" [1 1]
        (fn [args]
          (let [[r i] (cexp (complex-arg (nth args 0)))]
            (val/string (format-complex r i)))))

(defn- cln [[ar ai]]
  (when (and (zero? ar) (zero? ai)) (f/domain-error! :num))
  [(Math/log (Math/hypot ar ai))
   (Math/atan2 ai ar)])

(reg-im "IMLN" [1 1]
        (fn [args]
          (let [[r i] (cln (complex-arg (nth args 0)))]
            (val/string (format-complex r i)))))

(reg-im "IMLOG10" [1 1]
        (fn [args]
          (let [[r i] (cln (complex-arg (nth args 0)))
                ln10 (Math/log 10.0)]
            (val/string (format-complex (/ r ln10) (/ i ln10))))))

(reg-im "IMLOG2" [1 1]
        (fn [args]
          (let [[r i] (cln (complex-arg (nth args 0)))
                ln2 (Math/log 2.0)]
            (val/string (format-complex (/ r ln2) (/ i ln2))))))

(reg-im "IMPOWER" [2 2]
        (fn [args]
          (let [z (complex-arg (nth args 0))
                n (f/num! (nth args 1))
          ;; z^n = exp(n * ln(z))
                [lr li] (cln z)
                [tr ti] [(* n lr) (* n li)]
                [r i] (cexp [tr ti])]
            (val/string (format-complex r i)))))

(reg-im "IMSQRT" [1 1]
        (fn [args]
          (let [[r i] (complex-arg (nth args 0))
                m (Math/hypot r i)
                nr (Math/sqrt (/ (+ m r) 2.0))
                ni (* (if (neg? i) -1.0 1.0) (Math/sqrt (/ (- m r) 2.0)))]
            (val/string (format-complex nr ni)))))

(reg-im "IMCOS" [1 1]
        (fn [args]
          (let [[r i] (complex-arg (nth args 0))]
            (val/string (format-complex (* (Math/cos r) (Math/cosh i))
                                        (* -1.0 (Math/sin r) (Math/sinh i)))))))

(reg-im "IMSIN" [1 1]
        (fn [args]
          (let [[r i] (complex-arg (nth args 0))]
            (val/string (format-complex (* (Math/sin r) (Math/cosh i))
                                        (* (Math/cos r) (Math/sinh i)))))))

;; ---------------------------------------------------------------------------
;; Bessel functions — port of IronCalc's polynomial approximations
;; (Abramowitz & Stegun 9.4.*, 9.8.*). Low-precision — tests accept ~1e-6
;; relative error, same as IronCalc's EPS_LOW.
;;
;; BESSELI (modified, first kind)  uses A&S 9.8.1/9.8.2 polynomials + downward
;;                                  Miller recurrence for n >= 2
;; BESSELK (modified, second kind)  uses A&S 9.8.5/9.8.7 polynomials + upward
;;                                  recurrence for n >= 2
;; BESSELJ (first kind)             uses A&S 9.4.1/9.4.3 polynomials + upward
;;                                  recurrence for n >= 2
;; BESSELY (second kind)            uses A&S 9.4.2/9.4.4 polynomials + upward
;;                                  recurrence for n >= 2

(defn- bessel-i0 ^double [^double x]
  (let [ax (Math/abs x)]
    (cond
      (not (p/finite? x)) 0.0
      (< ax 3.75)
      (let [xx (/ x 3.75)
            y  (* xx xx)]
        (+ 1.0 (* y (+ 3.5156229
                       (* y (+ 3.0899424
                               (* y (+ 1.2067492
                                       (* y (+ 0.2659732
                                               (* y (+ 3.60768e-2
                                                       (* y 4.5813e-3)))))))))))))
      :else
      (let [y (/ 3.75 ax)]
        (* (/ (Math/exp ax) (Math/sqrt ax))
           (+ 0.39894228
              (* y (+ 1.328592e-2
                      (* y (+ 2.25319e-3
                              (* y (+ -1.57565e-3
                                      (* y (+ 9.16281e-3
                                              (* y (+ -2.057706e-2
                                                      (* y (+ 2.635537e-2
                                                              (* y (+ -1.647633e-2
                                                                      (* y 3.92377e-3)))))))))))))))))))))

(defn- bessel-i1 ^double [^double x]
  (let [ax (Math/abs x)]
    (if (< ax 3.75)
      (let [xx (/ x 3.75)
            y  (* xx xx)]
        (* x (+ 0.5
                (* y (+ 0.87890594
                        (* y (+ 0.51498869
                                (* y (+ 0.15084934
                                        (* y (+ 2.658733e-2
                                                (* y (+ 3.01532e-3
                                                        (* y 3.2411e-4))))))))))))))
      (let [y (/ 3.75 ax)
            r (* (/ (Math/exp ax) (Math/sqrt ax))
                 (+ 0.39894228
                    (* y (+ -3.988024e-2
                            (* y (+ -3.62018e-3
                                    (* y (+ 1.63801e-3
                                            (* y (+ -1.031555e-2
                                                    (* y (+ 2.282967e-2
                                                            (* y (+ -2.895312e-2
                                                                    (* y (+ 1.787654e-2
                                                                            (* y -4.20059e-3)))))))))))))))))]
        (if (neg? x) (- r) r)))))

(defn- bessel-i ^double [^long n ^double x]
  (cond
    (neg? n)     NaN
    (== n 0)     (bessel-i0 x)
    (zero? x)    0.0
    (== n 1)     (bessel-i1 x)
    (> (Math/abs x) 1e10) 0.0
    :else
    (let [accuracy 40
          large    1e10
          small    1e-10
          tox      (/ 2.0 (Math/abs x))
          m        (* 2 (+ (long (Math/sqrt (double (* accuracy n)))) n))]
      (loop [j m, bip 0.0, bi 1.0, result 0.0]
        (if (zero? j)
          (let [r (* result (/ (bessel-i0 x) bi))]
            (if (and (neg? x) (odd? n)) (- r) r))
          (let [bip' bi
                bi'  (+ bip (* (double j) tox bi))]
            (if (> (Math/abs bi') large)
              (recur (dec j)
                     (* bip' small)
                     (* bi'  small)
                     (* (if (== j n) bip' result) small))
              (recur (dec j) bip' bi'
                     (if (== j n) bip' result)))))))))

(defn- bessel-k0 ^double [^double x]
  (cond
    (<= x 0.0) +Inf
    (<= x 2.0)
    (let [y (/ (* x x) 4.0)]
      (+ (* (- (Math/log (/ x 2.0))) (bessel-i0 x))
         -0.57721566
         (* y (+ 0.42278420
                 (* y (+ 0.23069756
                         (* y (+ 3.488590e-2
                                 (* y (+ 2.62698e-3
                                         (* y (+ 1.0750e-4
                                                 (* y 7.4e-6)))))))))))))
    :else
    (let [y (/ 2.0 x)]
      (* (/ (Math/exp (- x)) (Math/sqrt x))
         (+ 1.25331414
            (* y (+ -7.832358e-2
                    (* y (+ 2.189568e-2
                            (* y (+ -1.062446e-2
                                    (* y (+ 5.87872e-3
                                            (* y (+ -2.51540e-3
                                                    (* y 5.3208e-4))))))))))))))))

(defn- bessel-k1 ^double [^double x]
  (cond
    (<= x 0.0) NaN
    (<= x 2.0)
    (let [y (/ (* x x) 4.0)]
      (+ (* (Math/log (/ x 2.0)) (bessel-i1 x))
         (* (/ 1.0 x)
            (+ 1.0
               (* y (+ 0.15443144
                       (* y (+ -0.67278579
                               (* y (+ -0.18156897
                                       (* y (+ -1.919402e-2
                                               (* y (+ -1.10404e-3
                                                       (* y -4.686e-5)))))))))))))))
    :else
    (let [y (/ 2.0 x)]
      (* (/ (Math/exp (- x)) (Math/sqrt x))
         (+ 1.25331414
            (* y (+ 0.23498619
                    (* y (+ -3.655620e-2
                            (* y (+ 1.504268e-2
                                    (* y (+ -7.80353e-3
                                            (* y (+ 3.25614e-3
                                                    (* y -6.8245e-4))))))))))))))))

(defn- bessel-k ^double [^long n ^double x]
  (cond
    (or (<= x 0.0) (neg? n)) NaN
    (== n 0) (bessel-k0 x)
    (== n 1) (bessel-k1 x)
    :else
    (let [tox (/ 2.0 x)]
      (loop [j 1, bkm (bessel-k0 x), bk (bessel-k1 x)]
        (if (== j n) bk
            (recur (inc j) bk (+ bkm (* (double j) tox bk))))))))

(defn- bessel-j0 ^double [^double x]
  (let [ax (Math/abs x)]
    (if (< ax 8.0)
      (let [y  (* x x)
            r  (+ 57568490574.0
                  (* y (+ -13362590354.0
                          (* y (+ 651619640.7
                                  (* y (+ -11214424.18
                                          (* y (+ 77392.33017
                                                  (* y -184.9052456))))))))))
            s  (+ 57568490411.0
                  (* y (+ 1029532985.0
                          (* y (+ 9494680.718
                                  (* y (+ 59272.64853
                                          (* y (+ 267.8532712 y)))))))))]
        (/ r s))
      (let [z    (/ 8.0 ax)
            y    (* z z)
            p    (+ 1.0
                    (* y (+ -0.1098628627e-2
                            (* y (+ 0.2734510407e-4
                                    (* y (+ -0.2073370639e-5
                                            (* y 0.2093887211e-6))))))))
            q    (+ -0.1562499995e-1
                    (* y (+ 0.1430488765e-3
                            (* y (+ -0.6911147651e-5
                                    (* y (+ 0.7621095161e-6
                                            (* y -0.934945152e-7))))))))
            xx   (- ax 0.785398164)]
        (* (Math/sqrt (/ 0.636619772 ax))
           (- (* (Math/cos xx) p) (* z (Math/sin xx) q)))))))

(defn- bessel-j1 ^double [^double x]
  (let [ax (Math/abs x)]
    (if (< ax 8.0)
      (let [y  (* x x)
            r  (* x
                  (+ 72362614232.0
                     (* y (+ -7895059235.0
                             (* y (+ 242396853.1
                                     (* y (+ -2972611.439
                                             (* y (+ 15704.48260
                                                     (* y -30.16036606)))))))))))
            s  (+ 144725228442.0
                  (* y (+ 2300535178.0
                          (* y (+ 18583304.74
                                  (* y (+ 99447.43394
                                          (* y (+ 376.9991397 y)))))))))]
        (/ r s))
      (let [z    (/ 8.0 ax)
            y    (* z z)
            p    (+ 1.0
                    (* y (+ 0.183105e-2
                            (* y (+ -0.3516396496e-4
                                    (* y (+ 0.2457520174e-5
                                            (* y -0.240337019e-6))))))))
            q    (+ 0.04687499995
                    (* y (+ -0.2002690873e-3
                            (* y (+ 0.8449199096e-5
                                    (* y (+ -0.88228987e-6
                                            (* y 0.105787412e-6))))))))
            xx   (- ax 2.356194491)
            r    (* (Math/sqrt (/ 0.636619772 ax))
                    (- (* (Math/cos xx) p) (* z (Math/sin xx) q)))]
        (if (neg? x) (- r) r)))))

(defn- bessel-y0 ^double [^double x]
  (if (< x 8.0)
    (let [y  (* x x)
          r  (+ -2957821389.0
                (* y (+ 7062834065.0
                        (* y (+ -512359803.6
                                (* y (+ 10879881.29
                                        (* y (+ -86327.92757
                                                (* y 228.4622733))))))))))
          s  (+ 40076544269.0
                (* y (+ 745249964.8
                        (* y (+ 7189466.438
                                (* y (+ 47447.26470
                                        (* y (+ 226.1030244 y)))))))))]
      (+ (/ r s) (* 0.636619772 (bessel-j0 x) (Math/log x))))
    (let [z    (/ 8.0 x)
          y    (* z z)
          p    (+ 1.0
                  (* y (+ -0.1098628627e-2
                          (* y (+ 0.2734510407e-4
                                  (* y (+ -0.2073370639e-5
                                          (* y 0.2093887211e-6))))))))
          q    (+ -0.1562499995e-1
                  (* y (+ 0.1430488765e-3
                          (* y (+ -0.6911147651e-5
                                  (* y (+ 0.7621095161e-6
                                          (* y -0.934945152e-7))))))))
          xx   (- x 0.785398164)]
      (* (Math/sqrt (/ 0.636619772 x))
         (+ (* (Math/sin xx) p) (* z (Math/cos xx) q))))))

(defn- bessel-y1 ^double [^double x]
  (if (< x 8.0)
    (let [y  (* x x)
          r  (* x
                (+ -0.4900604943e13
                   (* y (+ 0.1275274390e13
                           (* y (+ -0.5153438139e11
                                   (* y (+ 0.7349264551e9
                                           (* y (+ -0.4237922726e7
                                                   (* y 0.8511937935e4)))))))))))
          s  (+ 0.2499580570e14
                (* y (+ 0.4244419664e12
                        (* y (+ 0.3733650367e10
                                (* y (+ 0.2245904002e8
                                        (* y (+ 0.1020426050e6
                                                (* y (+ 0.3549632885e3 y)))))))))))]
      (+ (/ r s) (* 0.636619772 (- (* (bessel-j1 x) (Math/log x)) (/ 1.0 x)))))
    (let [z    (/ 8.0 x)
          y    (* z z)
          p    (+ 1.0
                  (* y (+ 0.183105e-2
                          (* y (+ -0.3516396496e-4
                                  (* y (+ 0.2457520174e-5
                                          (* y -0.240337019e-6))))))))
          q    (+ 0.04687499995
                  (* y (+ -0.2002690873e-3
                          (* y (+ 0.8449199096e-5
                                  (* y (+ -0.88228987e-6
                                          (* y 0.105787412e-6))))))))
          xx   (- x 2.356194491)]
      (* (Math/sqrt (/ 0.636619772 x))
         (+ (* (Math/sin xx) p) (* z (Math/cos xx) q))))))

(defn- bessel-j ^double [^long n ^double x]
  (cond
    (neg? n)     NaN
    (== n 0)     (bessel-j0 x)
    (== n 1)     (bessel-j1 x)
    (zero? x)    0.0
    :else
    (let [ax  (Math/abs x)
          tox (/ 2.0 ax)]
      (if (> ax (double n))
        ;; upward recurrence is stable
        (let [r (loop [j 1, bjm (bessel-j0 ax), bj (bessel-j1 ax)]
                  (if (== j n) bj
                      (recur (inc j) bj (- (* (double j) tox bj) bjm))))]
          (if (and (neg? x) (odd? n)) (- r) r))
        ;; downward (Miller's) for x < n
        (let [acc   40
              m     (* 2 (quot (+ n (long (Math/sqrt (double (* acc n))))) 2))
              big   1.0e10
              small 1.0e-10]
          (loop [j m, jsum false, bjp 0.0, bj 1.0, sum 0.0, result 0.0]
            (if (zero? j)
              (let [sum'    (- (* 2.0 sum) bj)
                    ans     (/ result sum')]
                (if (and (neg? x) (odd? n)) (- ans) ans))
              (let [bjp' bj
                    bj'  (- (* (double j) tox bj) bjp)
                    over (> (Math/abs bj') big)
                    bjp' (if over (* bjp' small) bjp')
                    bj'  (if over (* bj' small) bj')
                    result (if over (* result small) result)
                    sum    (if over (* sum small) sum)
                    sum'   (if jsum (+ sum bj') sum)
                    result (if (== (dec j) n) bjp' result)]
                (recur (dec j) (not jsum) bjp' bj' sum' result)))))))))

(defn- bessel-y ^double [^long n ^double x]
  (cond
    (or (neg? n) (<= x 0.0)) NaN
    (== n 0) (bessel-y0 x)
    (== n 1) (bessel-y1 x)
    :else
    (let [tox (/ 2.0 x)]
      (loop [j 1, bym (bessel-y0 x), by (bessel-y1 x)]
        (if (== j n) by
            (recur (inc j) by (- (* (double j) tox by) bym)))))))

(defn- bessel-check [^double r]
  (if (or (not (p/finite? r)) (p/nan? r))
    (f/domain-error! :num)
    (val/number r)))

(f/register! "BESSELJ"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     n (long (f/num! (nth args 1)))]
                 (when (neg? n) (f/domain-error! :num))
                 (bessel-check (bessel-j n x))))
             :arity [2 2])

(f/register! "BESSELY"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     n (long (f/num! (nth args 1)))]
                 (when (neg? n) (f/domain-error! :num))
                 (bessel-check (bessel-y n x))))
             :arity [2 2])

(f/register! "BESSELI"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     n (long (f/num! (nth args 1)))]
                 (bessel-check (bessel-i n x))))
             :arity [2 2])

(f/register! "BESSELK"
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     n (long (f/num! (nth args 1)))]
                 (bessel-check (bessel-k n x))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; CONVERT — unit conversion across weight/distance/time/pressure/force/
;; energy/power/magnetism/volume/area/information/speed/temperature.
;; Ported from IronCalc. Each kind is a lookup table of unit→SI-value.
;; Converting value from A→B: (value * SI(A)) / SI(B). Area prefixes square,
;; volume prefixes cube. Temperature is affine so it gets special handling.

(def ^:private convert-prefixes
  {"Y" 1e24 "Z" 1e21 "E" 1e18 "P" 1e15 "T" 1e12
   "G" 1e9  "M" 1e6  "k" 1e3  "h" 1e2  "da" 10.0 "e" 10.0
   "d" 0.1  "c" 0.01 "m" 0.001 "u" 1e-6 "n" 1e-9
   "p" 1e-12 "f" 1e-15 "a" 1e-18 "z" 1e-21 "y" 1e-24
   "Yi" (Math/pow 2 80) "Zi" (Math/pow 2 70) "Ei" (Math/pow 2 60)
   "Pi" (Math/pow 2 50) "Ti" (Math/pow 2 40) "Gi" (Math/pow 2 30)
   "Mi" (Math/pow 2 20) "ki" (Math/pow 2 10)})

(def ^:private convert-units
  {"weight"      {"g" 1.0 "sg" 14593.9029372064 "lbm" 453.59237
                  "u" 1.660538782e-24 "ozm" 28.349523125
                  "grain" 0.06479891 "cwt" 45359.237 "shweight" 45359.237
                  "uk_cwt" 50802.34544 "lcwt" 50802.34544
                  "stone" 6350.29318 "ton" 907184.74
                  "brton" 1016046.9088 "LTON" 1016046.9088 "uk_ton" 1016046.9088}
   "distance"    {"m" 1.0 "mi" 1609.344 "Nmi" 1852.0
                  "in" 0.0254 "ft" 0.3048 "yd" 0.9144
                  "ang" 1e-10 "ell" 1.143
                  "ly" 9460730472580800.0 "parsec" 30856775812815500.0
                  "pc" 30856775812815500.0
                  "Picapt" 3.52777777777778e-4 "Pica" 3.52777777777778e-4
                  "pica" 4.23333333333333e-3 "survey_mi" 1609.34721869444}
   "time"        {"yr" 31557600.0 "day" 86400.0 "d" 86400.0
                  "hr" 3600.0 "mn" 60.0 "min" 60.0 "sec" 1.0 "s" 1.0}
   "pressure"    {"Pa" 1.0 "p" 1.0 "atm" 101325.0 "at" 101325.0
                  "mmHg" 133.322 "psi" 6894.75729316836
                  "Torr" 133.322368421053}
   "force"       {"N" 1.0 "dyn" 1e-5 "dy" 1e-5
                  "lbf" 4.4482216152605 "pond" 9.80665e-3}
   "energy"      {"J" 1.0 "e" 1e-7 "c" 4.184 "cal" 4.1868
                  "eV" 1.602176487e-19 "ev" 1.602176487e-19
                  "HPh" 2684519.53769617 "hh" 2684519.53769617
                  "Wh" 3600.0 "wh" 3600.0
                  "flb" 1.3558179483314 "BTU" 1055.05585262 "btu" 1055.05585262}
   "power"       {"HP" 745.69987158227 "h" 745.69987158227
                  "PS" 735.49875 "W" 1.0 "w" 1.0}
   "magnetism"   {"T" 1.0 "ga" 1e-4}
   "volume"      {"tsp" 4.92892159375e-6 "tspm" 5e-6
                  "tbs" 1.478676478125e-5 "oz" 2.95735295625e-5
                  "cup" 2.365882365e-4
                  "pt" 4.73176473e-4 "us_pt" 4.73176473e-4
                  "uk_pt" 5.6826125e-4
                  "qt" 9.46352946e-4 "uk_qt" 1.1365225e-3
                  "gal" 3.785411784e-3 "uk_gal" 4.54609e-3
                  "l" 1e-3 "L" 1e-3 "lt" 1e-3
                  "ang3" 1e-30 "ang^3" 1e-30
                  "barrel" 0.158987294928 "bushel" 3.523907016688e-2
                  "ft3" 2.8316846592e-2 "ft^3" 2.8316846592e-2
                  "in3" 1.6387064e-5 "in^3" 1.6387064e-5
                  "ly3" 8.46786664623715e47 "ly^3" 8.46786664623715e47
                  "m3" 1.0 "m^3" 1.0
                  "mi3" 4168181825.44058 "mi^3" 4168181825.44058
                  "yd3" 0.764554857984 "yd^3" 0.764554857984
                  "Nmi3" 6352182208.0 "Nmi^3" 6352182208.0
                  "Picapt3" 4.39039566186557e-11 "Picapt^3" 4.39039566186557e-11
                  "Pica3" 4.39039566186557e-11 "Pica^3" 4.39039566186557e-11
                  "GRT" 2.8316846592 "regton" 2.8316846592
                  "MTON" 1.13267386368}
   "area"        {"uk_acre" 4046.8564224 "us_acre" 4046.87260987425
                  "ang2" 1e-20 "ang^2" 1e-20
                  "ar" 100.0
                  "ft2" 9.290304e-2 "ft^2" 9.290304e-2
                  "ha" 10000.0
                  "in2" 6.4516e-4 "in^2" 6.4516e-4
                  "ly2" 8.95054210748189e31 "ly^2" 8.95054210748189e31
                  "m2" 1.0 "m^2" 1.0
                  "Morgen" 2500.0
                  "mi2" 2589988.110336 "mi^2" 2589988.110336
                  "Nmi2" 3429904.0 "Nmi^2" 3429904.0
                  "Picapt2" 1.24452160493827e-7 "Pica2" 1.24452160493827e-7
                  "Pica^2" 1.24452160493827e-7 "Picapt^2" 1.24452160493827e-7
                  "yd2" 0.83612736 "yd^2" 0.83612736}
   "information" {"bit" 1.0 "byte" 8.0}
   "speed"       {"admkn" 0.514773333333333 "kn" 0.514444444444444
                  "m/h" 2.77777777777778e-4 "m/hr" 2.77777777777778e-4
                  "m/s" 1.0 "m/sec" 1.0 "mph" 0.44704}
   "temperature" {"C" 1.0 "cel" 1.0 "F" 1.0 "fah" 1.0
                  "K" 1.0 "kel" 1.0 "Rank" 1.0 "Reau" 1.0}})

(def ^:private convert-mks
  ;; units that admit SI prefixes (no "kC" for kilo-Celsius)
  #{"Pa" "p" "atm" "at" "mmHg" "g" "u" "m" "ang" "ly" "parsec" "pc"
    "ang2" "ang^2" "ar" "m2" "m^2"
    "N" "dyn" "dy" "pond"
    "J" "e" "c" "cal" "eV" "ev" "Wh" "wh"
    "W" "w" "T" "ga"
    "uk_pt" "l" "L" "lt" "ang3" "ang^3" "m3" "m^3"
    "bit" "byte" "m/h" "m/hr" "m/s" "m/sec" "mph" "K" "kel"})

(def ^:private convert-volumes #{"ang3" "ang^3" "m3" "m^3"})

(def ^:private convert-all-units
  (set (mapcat keys (vals convert-units))))

(defn- convert-lookup
  "Return [kind unit-name si-value] or nil. First try exact match across all
  kinds; fall back to stripping any prefix off a prefixable (mks) unit."
  [^String s]
  (or (some (fn [[kind table]]
              (when-let [v (get table s)]
                [kind s v]))
            convert-units)
      (when-not (contains? convert-all-units s)
        (some (fn [[kind table]]
                (some (fn [[uname uval]]
                        (when (and (contains? convert-mks uname)
                                   (str/ends-with? s uname))
                          (let [pk (subs s 0 (- (count s) (count uname)))]
                            (when-let [mod (get convert-prefixes pk)]
                              (let [scale (cond
                                            (and (= kind "area")
                                                 (not= uname "ar"))
                                            (* mod mod)
                                            (and (= kind "volume")
                                                 (contains? convert-volumes uname))
                                            (* mod mod mod)
                                            :else mod)]
                                [kind uname (* scale uval)])))))
                      table))
              convert-units))))

(defn- to-kelvin ^double [^String u ^double v]
  (case u
    ("K" "kel")  v
    ("C" "cel")  (+ v 273.15)
    "Rank"       (* (/ 5.0 9.0) v)
    "Reau"       (+ (* (/ 5.0 4.0) v) 273.15)
    ("F" "fah")  (* (/ 5.0 9.0) (+ v 459.67))))

(defn- from-kelvin ^double [^String u ^double k]
  (case u
    ("K" "kel")  k
    ("C" "cel")  (- k 273.15)
    "Rank"       (* (/ 9.0 5.0) k)
    "Reau"       (* (/ 4.0 5.0) (- k 273.15))
    ("F" "fah")  (- (* (/ 9.0 5.0) k) 459.67)))

(f/register! "CONVERT"
             ^{:scalar? true}
             (fn [args]
               (let [value (f/num! (nth args 0))
                     from  (str (:v (nth args 1)))
                     to    (str (:v (nth args 2)))
                     fl    (convert-lookup from)
                     tl    (convert-lookup to)]
                 (cond
                   (or (nil? fl) (nil? tl))       val/ERR-NA
                   (not= (first fl) (first tl))   val/ERR-NA
                   (= "temperature" (first fl))
                   (val/number
                    (from-kelvin (second tl)
                                 (to-kelvin (second fl) value)))
                   :else
                   (val/number (/ (* value (nth fl 2)) (nth tl 2))))))
             :arity [3 3])
