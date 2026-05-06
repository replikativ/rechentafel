(ns rechentafel.fn.financial
  "Financial functions (POI category: financial — 46 fns).

  Covers the widely-used ones fully:
    - Annuity primitives: FV, PV, PMT, NPER, RATE
    - Interest/principal: IPMT, PPMT, CUMIPMT, CUMPRINC, ISPMT
    - Cash flows: NPV, IRR, MIRR, XNPV, XIRR
    - Depreciation: SLN, SYD, DB, DDB, VDB
    - Rate conversion: EFFECT, NOMINAL
    - T-bills / simple discount: TBILLEQ, TBILLPRICE, TBILLYIELD,
      RECEIVED, DISC, INTRATE, ACCRINTM

  POI itself leaves bond-math (PRICE/YIELD/COUP*/DURATION/MDURATION/
  AMORDEGRC/AMORLINC/ACCRINT) NotImplemented because they depend on a
  full day-count-convention implementation — we register them as #N/A
  stubs so formulas parse cleanly rather than silently succeeding with
  wrong numbers.

  All annuity functions follow the Excel formula:

    PV*(1+r)^n + PMT*(1 + r*type)*((1+r)^n - 1)/r + FV = 0

  with the `type` arg meaning 0 = end-of-period, 1 = beginning. When
  r=0 this degenerates to PV + PMT*n + FV = 0."
  (:require [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Core annuity formula

(defn- annuity-fv
  "FV given rate, nper, pmt, pv, type (0 or 1)."
  [r n pmt pv t]
  (if (zero? r)
    (- (- pv) (* pmt n))
    (let [g (Math/pow (+ 1.0 r) n)]
      (- (- (* pv g))
         (* pmt (+ 1.0 (* r t)) (/ (- g 1.0) r))))))

(defn- annuity-pv
  [r n pmt fv t]
  (if (zero? r)
    (- (- fv) (* pmt n))
    (let [g (Math/pow (+ 1.0 r) n)]
      (/ (- (- fv)
            (* pmt (+ 1.0 (* r t)) (/ (- g 1.0) r)))
         g))))

(defn- annuity-pmt
  [r n pv fv t]
  (if (zero? r)
    (- (/ (+ pv fv) n))
    (let [g (Math/pow (+ 1.0 r) n)]
      (/ (- (- fv) (* pv g))
         (* (+ 1.0 (* r t)) (/ (- g 1.0) r))))))

(defn- annuity-nper
  [r pmt pv fv t]
  (if (zero? r)
    (- (/ (+ pv fv) pmt))
    (let [num (- (* pmt (+ 1.0 (* r t)))
                 (* fv r))
          den (+ (* pv r) (* pmt (+ 1.0 (* r t))))]
      (when (or (zero? den) (<= (/ num den) 0.0))
        (f/domain-error! :num))
      (/ (Math/log (/ num den))
         (Math/log (+ 1.0 r))))))

(defn- annuity-residual
  "f(r) = PV*(1+r)^n + PMT*(1+r*t)*((1+r)^n-1)/r + FV. Returns 0 at the
  solved rate. Uses the limit form when r ≈ 0 to avoid div-by-zero."
  [r n pmt pv fv t]
  (if (< (Math/abs (double r)) 1.0e-12)
    (+ pv (* pmt n) fv)
    (let [g (Math/pow (+ 1.0 r) n)]
      (+ (* pv g)
         (* pmt (+ 1.0 (* r t)) (/ (- g 1.0) r))
         fv))))

(defn- annuity-rate
  "Find r by Newton-Raphson. `guess` is the starting estimate. Bails
  after max-iter iterations with #NUM!."
  [n pmt pv fv t guess]
  (let [max-iter 60 eps 1.0e-10 h 1.0e-6]
    (loop [r guess i 0]
      (when (> i max-iter) (f/domain-error! :num))
      (let [fr  (annuity-residual r n pmt pv fv t)
            fr' (/ (- (annuity-residual (+ r h) n pmt pv fv t)
                      (annuity-residual (- r h) n pmt pv fv t))
                   (* 2.0 h))]
        (if (< (Math/abs (double fr)) eps) r
            (if (zero? fr') (f/domain-error! :num)
                (let [dr (/ fr fr')
                      r' (- r dr)]
                  (if (< (Math/abs (double dr)) eps) r'
                      (recur r' (inc i))))))))))

(defn- opt-arg
  "Optional arg at index i: returns `default` when missing or blank."
  [args i default]
  (if (or (>= i (count args))
          (val/blank? (nth args i)))
    default
    (f/num! (nth args i))))

(f/register! "FV"
             ^{:scalar? true}
             (fn [args]
               (let [r   (f/num! (nth args 0))
                     n   (f/num! (nth args 1))
                     pmt (f/num! (nth args 2))
                     pv  (opt-arg args 3 0.0)
                     t   (opt-arg args 4 0.0)]
                 (val/number (annuity-fv r n pmt pv t))))
             :arity [3 5])

(f/register! "PV"
             ^{:scalar? true}
             (fn [args]
               (let [r   (f/num! (nth args 0))
                     n   (f/num! (nth args 1))
                     pmt (f/num! (nth args 2))
                     fv  (opt-arg args 3 0.0)
                     t   (opt-arg args 4 0.0)]
                 (val/number (annuity-pv r n pmt fv t))))
             :arity [3 5])

(f/register! "PMT"
             ^{:scalar? true}
             (fn [args]
               (let [r  (f/num! (nth args 0))
                     n  (f/num! (nth args 1))
                     pv (f/num! (nth args 2))
                     fv (opt-arg args 3 0.0)
                     t  (opt-arg args 4 0.0)]
                 (val/number (annuity-pmt r n pv fv t))))
             :arity [3 5])

(f/register! "NPER"
             ^{:scalar? true}
             (fn [args]
               (let [r   (f/num! (nth args 0))
                     pmt (f/num! (nth args 1))
                     pv  (f/num! (nth args 2))
                     fv  (opt-arg args 3 0.0)
                     t   (opt-arg args 4 0.0)]
                 (val/number (annuity-nper r pmt pv fv t))))
             :arity [3 5])

(f/register! "RATE"
             ^{:scalar? true}
             (fn [args]
               (let [n   (f/num! (nth args 0))
                     pmt (f/num! (nth args 1))
                     pv  (f/num! (nth args 2))
                     fv  (opt-arg args 3 0.0)
                     t   (opt-arg args 4 0.0)
                     guess (opt-arg args 5 0.1)]
                 (val/number (annuity-rate n pmt pv fv t guess))))
             :arity [3 6])

;; ---------------------------------------------------------------------------
;; IPMT / PPMT / CUMIPMT / CUMPRINC

(defn- pmt-split
  "For period `per` (1-based) of a loan with rate r, n periods, pv, fv,
  type t: returns [ipmt ppmt]."
  [r per n pv fv t]
  (let [pmt (annuity-pmt r n pv fv t)
        ;; FV at end of period (per-1) — the *remaining* balance's cost
        balance-start (annuity-fv r (- per 1.0) pmt pv t)
        ;; When type=1 and per=1, first payment is *before* interest accrues
        ipmt (if (and (= t 1.0) (= per 1.0))
               0.0
               (let [base (* balance-start r)]
                 (if (= t 1.0) (/ base (+ 1.0 r)) base)))
        ppmt (- pmt ipmt)]
    [ipmt ppmt]))

(f/register! "IPMT"
             ^{:scalar? true}
             (fn [args]
               (let [r   (f/num! (nth args 0))
                     per (f/num! (nth args 1))
                     n   (f/num! (nth args 2))
                     pv  (f/num! (nth args 3))
                     fv  (opt-arg args 4 0.0)
                     t   (opt-arg args 5 0.0)]
                 (val/number (first (pmt-split r per n pv fv t)))))
             :arity [4 6])

(f/register! "PPMT"
             ^{:scalar? true}
             (fn [args]
               (let [r   (f/num! (nth args 0))
                     per (f/num! (nth args 1))
                     n   (f/num! (nth args 2))
                     pv  (f/num! (nth args 3))
                     fv  (opt-arg args 4 0.0)
                     t   (opt-arg args 5 0.0)]
                 (val/number (second (pmt-split r per n pv fv t)))))
             :arity [4 6])

(f/register! "ISPMT"
  ;; Simple interest for period `per` on a straight-line amortisation:
  ;; ISPMT(rate, per, nper, pv) = pv * rate * (per/nper - 1)
             ^{:scalar? true}
             (fn [args]
               (let [r   (f/num! (nth args 0))
                     per (f/num! (nth args 1))
                     n   (f/num! (nth args 2))
                     pv  (f/num! (nth args 3))]
                 (val/number (* pv r (- (/ per n) 1.0)))))
             :arity [4 4])

(f/register! "CUMIPMT"
             ^{:scalar? true}
             (fn [args]
               (let [r (f/num! (nth args 0))
                     n (f/num! (nth args 1))
                     pv (f/num! (nth args 2))
                     start (long (f/num! (nth args 3)))
                     end   (long (f/num! (nth args 4)))
                     t     (f/num! (nth args 5))]
                 (when (or (< start 1) (< end start) (> end (long n)))
                   (f/domain-error! :num))
                 (val/number
                  (reduce + 0.0
                          (for [p (range start (inc end))]
                            (first (pmt-split r (double p) n pv 0.0 t)))))))
             :arity [6 6])

(f/register! "CUMPRINC"
             ^{:scalar? true}
             (fn [args]
               (let [r (f/num! (nth args 0))
                     n (f/num! (nth args 1))
                     pv (f/num! (nth args 2))
                     start (long (f/num! (nth args 3)))
                     end   (long (f/num! (nth args 4)))
                     t     (f/num! (nth args 5))]
                 (when (or (< start 1) (< end start) (> end (long n)))
                   (f/domain-error! :num))
                 (val/number
                  (reduce + 0.0
                          (for [p (range start (inc end))]
                            (second (pmt-split r (double p) n pv 0.0 t)))))))
             :arity [6 6])

;; ---------------------------------------------------------------------------
;; NPV / IRR / MIRR / XNPV / XIRR

(defn- flatten-numerics
  "Collect all numerics from args (areas expanded), preserving order.
  Errors propagate."
  [args]
  (f/collect-finite-numerics args))

(f/register! "NPV"
  ;; NPV(rate, value1, value2, ...) = Σ v_i / (1+rate)^i
             (fn [args]
               (let [r (f/num! (nth args 0))
                     vs (flatten-numerics (rest args))]
                 (val/number
                  (loop [i 1 acc 0.0 vs vs]
                    (if (empty? vs) acc
                        (recur (inc i)
                               (+ acc (/ (double (first vs))
                                         (Math/pow (+ 1.0 r) i)))
                               (rest vs)))))))
             :arity [2 254])

(defn- npv-at
  "NPV using 0-indexed convention: Σ v_i / (1+r)^i for i=0..n-1 — used
  by IRR's residual."
  ^double [^double r vs]
  (let [acc (volatile! 0.0)]
    (dotimes [i (count vs)]
      (vswap! acc + (/ (double (nth vs i))
                       (Math/pow (+ 1.0 r) i))))
    @acc))

(f/register! "IRR"
  ;; IRR(values, [guess]) solves 0 = Σ v_i / (1+r)^i for i=0..n-1.
  ;; Newton-Raphson starting from guess (default 0.1).
             (fn [args]
               (let [vs (flatten-numerics [(nth args 0)])
                     guess (opt-arg args 1 0.1)]
                 (when (or (empty? vs)
                           (not (and (some pos? vs) (some neg? vs))))
                   (f/domain-error! :num))
                 (let [max-iter 60 eps 1.0e-8 h 1.0e-7]
                   (val/number
                    (loop [r guess i 0]
                      (when (> i max-iter) (f/domain-error! :num))
                      (let [fr (npv-at r vs)
                            fr' (/ (- (npv-at (+ r h) vs)
                                      (npv-at (- r h) vs))
                                   (* 2.0 h))]
                        (if (< (Math/abs fr) eps) r
                            (if (zero? fr') (f/domain-error! :num)
                                (let [dr (/ fr fr')
                                      r' (- r dr)]
                                  (if (<= r' -1.0) (recur (/ (- r 1.0) 2.0) (inc i))
                                      (if (< (Math/abs dr) eps) r'
                                          (recur r' (inc i)))))))))))))
             :arity [1 2])

(f/register! "MIRR"
             (fn [args]
               (let [vs (flatten-numerics [(nth args 0)])
                     finance (f/num! (nth args 1))
                     reinvest (f/num! (nth args 2))
                     n (count vs)
                     pos-sum (reduce + 0.0
                                     (map-indexed (fn [i v]
                                                    (if (pos? v)
                                                      (* v (Math/pow (+ 1.0 reinvest)
                                                                     (- (dec n) i)))
                                                      0.0))
                                                  vs))
                     neg-sum (reduce + 0.0
                                     (map-indexed (fn [i v]
                                                    (if (neg? v)
                                                      (/ v (Math/pow (+ 1.0 finance) i))
                                                      0.0))
                                                  vs))]
                 (when (or (zero? neg-sum) (zero? pos-sum)) (f/domain-error! :div0))
                 (val/number
                  (- (Math/pow (/ (- pos-sum) neg-sum) (/ 1.0 (double (dec n))))
                     1.0))))
             :arity [3 3])

;; ---------------------------------------------------------------------------
;; XNPV / XIRR use Excel serial-date spacing (days/365).

(defn- collect-pair [a b]
  (let [xs (flatten-numerics [a])
        ds (flatten-numerics [b])]
    (when (not= (count xs) (count ds)) (f/domain-error! :num))
    [xs ds]))

(defn- xnpv-at ^double [^double r values dates]
  (let [d0 (double (first dates))
        acc (volatile! 0.0)]
    (dotimes [i (count values)]
      (vswap! acc +
              (/ (double (nth values i))
                 (Math/pow (+ 1.0 r)
                           (/ (- (double (nth dates i)) d0) 365.0)))))
    @acc))

(f/register! "XNPV"
             (fn [args]
               (let [r (f/num! (nth args 0))
                     [vs ds] (collect-pair (nth args 1) (nth args 2))]
                 (val/number (xnpv-at r vs ds))))
             :arity [3 3])

(f/register! "XIRR"
             (fn [args]
               (let [[vs ds] (collect-pair (nth args 0) (nth args 1))
                     guess   (opt-arg args 2 0.1)]
                 (when (or (empty? vs)
                           (not (and (some pos? vs) (some neg? vs))))
                   (f/domain-error! :num))
                 (let [max-iter 60 eps 1.0e-8 h 1.0e-7]
                   (val/number
                    (loop [r guess i 0]
                      (when (> i max-iter) (f/domain-error! :num))
                      (let [fr (xnpv-at r vs ds)
                            fr' (/ (- (xnpv-at (+ r h) vs ds)
                                      (xnpv-at (- r h) vs ds))
                                   (* 2.0 h))]
                        (if (< (Math/abs fr) eps) r
                            (if (zero? fr') (f/domain-error! :num)
                                (let [dr (/ fr fr')
                                      r' (- r dr)]
                                  (if (<= r' -1.0) (recur (/ (- r 1.0) 2.0) (inc i))
                                      (if (< (Math/abs dr) eps) r'
                                          (recur r' (inc i)))))))))))))
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; Depreciation

(f/register! "SLN"
  ;; Straight-line: (cost - salvage) / life
             ^{:scalar? true}
             (fn [args]
               (let [cost (f/num! (nth args 0))
                     salv (f/num! (nth args 1))
                     life (f/num! (nth args 2))]
                 (when (zero? life) (f/domain-error! :div0))
                 (val/number (/ (- cost salv) life))))
             :arity [3 3])

(f/register! "SYD"
  ;; Sum-of-Years' Digits: (cost - salvage) * (life - per + 1) /
  ;; (life * (life + 1) / 2)
             ^{:scalar? true}
             (fn [args]
               (let [cost (f/num! (nth args 0))
                     salv (f/num! (nth args 1))
                     life (f/num! (nth args 2))
                     per  (f/num! (nth args 3))]
                 (when (or (<= life 0.0) (<= per 0.0) (> per life))
                   (f/domain-error! :num))
                 (val/number
                  (/ (* (- cost salv) (- (inc life) per))
                     (/ (* life (inc life)) 2.0)))))
             :arity [4 4])

(defn- db-rate ^double [^double cost ^double salv ^double life]
  (- 1.0 (Math/pow (/ salv cost) (/ 1.0 life))))

(f/register! "DB"
  ;; Fixed-declining-balance. Rate is rounded to 3 dp; first/last year
  ;; are prorated by `month` (default 12). Matches Excel semantics.
             ^{:scalar? true}
             (fn [args]
               (let [cost (f/num! (nth args 0))
                     salv (f/num! (nth args 1))
                     life (f/num! (nth args 2))
                     per  (f/num! (nth args 3))
                     mon  (opt-arg args 4 12.0)]
                 (when (or (<= cost 0.0) (< salv 0.0) (<= life 0.0) (<= per 0.0))
                   (f/domain-error! :num))
                 (let [rate (/ (Math/round (* (db-rate cost salv life) 1000.0)) 1000.0)
                       first-year (/ (* cost rate mon) 12.0)]
                   (val/number
                    (cond
                      (= per 1.0) first-year
                      (= per (Math/ceil life))
           ;; partial last year
                      (let [acc-before (loop [p 2 acc first-year]
                                         (if (>= p per) acc
                                             (recur (inc p)
                                                    (+ acc (* rate (- cost acc))))))]
                        (* (- cost acc-before) rate (/ (- 12.0 mon) 12.0)))
                      :else
                      (let [acc (loop [p 2 acc first-year]
                                  (if (>= p per) acc
                                      (recur (inc p)
                                             (+ acc (* rate (- cost acc))))))]
                        (* rate (- cost acc))))))))
             :arity [4 5])

(f/register! "DDB"
  ;; Double-declining-balance (configurable factor, default 2).
             ^{:scalar? true}
             (fn [args]
               (let [cost (f/num! (nth args 0))
                     salv (f/num! (nth args 1))
                     life (f/num! (nth args 2))
                     per  (f/num! (nth args 3))
                     fact (opt-arg args 4 2.0)]
                 (when (or (<= life 0.0) (<= per 0.0) (> per life))
                   (f/domain-error! :num))
                 (let [rate (/ fact life)
                       acc  (loop [p 1 book cost acc 0.0]
                              (if (> p (dec per)) [book acc]
                                  (let [d (min (* book rate)
                                               (max 0.0 (- book salv)))]
                                    (recur (inc p) (- book d) (+ acc d)))))
                       [book _] acc]
                   (val/number (min (* book rate) (max 0.0 (- book salv)))))))
             :arity [4 5])

(f/register! "VDB"
  ;; Variable-declining-balance over a [start, end] period range. When
  ;; no_switch is TRUE we stay with DDB; otherwise we switch to SLN
  ;; once SLN > DDB for the remaining life. Integer periods only.
             ^{:scalar? true}
             (fn [args]
               (let [cost (f/num! (nth args 0))
                     salv (f/num! (nth args 1))
                     life (long (f/num! (nth args 2)))
                     start (long (f/num! (nth args 3)))
                     end   (long (f/num! (nth args 4)))
                     factor (opt-arg args 5 2.0)
                     no-switch? (if (> (count args) 6) (f/bool! (nth args 6)) false)]
                 (when (or (<= life 0) (< start 0) (< end start) (> end life))
                   (f/domain-error! :num))
                 (let [rate (/ factor life)
            ;; Simulate period-by-period over [1, life]; accumulate
            ;; depreciation between periods `start+1` and `end`.
                       deprec (loop [p 1 book cost acc 0.0 picked 0.0]
                                (if (> p life) picked
                                    (let [remaining (- life p)
                                          sln (when-not no-switch?
                                                (when (pos? remaining)
                                                  (/ (- book salv) remaining)))
                                          ddb (min (* book rate)
                                                   (max 0.0 (- book salv)))
                                          d (if (and sln (> sln ddb)) sln ddb)
                                          in-window? (and (> p start) (<= p end))]
                                      (recur (inc p)
                                             (- book d)
                                             (+ acc d)
                                             (if in-window? (+ picked d) picked)))))]
                   (val/number deprec))))
             :arity [5 7])

;; ---------------------------------------------------------------------------
;; Rate conversion

(f/register! "EFFECT"
             ^{:scalar? true}
             (fn [args]
               (let [nom (f/num! (nth args 0))
                     m   (f/num! (nth args 1))]
                 (when (or (<= nom 0.0) (< m 1.0)) (f/domain-error! :num))
                 (val/number (- (Math/pow (+ 1.0 (/ nom m)) m) 1.0))))
             :arity [2 2])

(f/register! "NOMINAL"
             ^{:scalar? true}
             (fn [args]
               (let [eff (f/num! (nth args 0))
                     m   (f/num! (nth args 1))]
                 (when (or (<= eff 0.0) (< m 1.0)) (f/domain-error! :num))
                 (val/number (* m (- (Math/pow (+ 1.0 eff) (/ 1.0 m)) 1.0)))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; T-bills and simple discount

(defn- days-between-serial ^double [^double a ^double b]
  (- b a))

(f/register! "TBILLEQ"
             ^{:scalar? true}
             (fn [args]
               (let [settle (f/num! (nth args 0))
                     mat    (f/num! (nth args 1))
                     disc   (f/num! (nth args 2))
                     days   (days-between-serial settle mat)]
                 (when (or (<= disc 0.0) (<= days 0.0) (> days 365.0))
                   (f/domain-error! :num))
                 (val/number (/ (* 365.0 disc) (- 360.0 (* disc days))))))
             :arity [3 3])

(f/register! "TBILLPRICE"
             ^{:scalar? true}
             (fn [args]
               (let [settle (f/num! (nth args 0))
                     mat    (f/num! (nth args 1))
                     disc   (f/num! (nth args 2))
                     days   (days-between-serial settle mat)]
                 (when (or (<= disc 0.0) (<= days 0.0) (> days 365.0))
                   (f/domain-error! :num))
                 (val/number (* 100.0 (- 1.0 (/ (* disc days) 360.0))))))
             :arity [3 3])

(f/register! "TBILLYIELD"
             ^{:scalar? true}
             (fn [args]
               (let [settle (f/num! (nth args 0))
                     mat    (f/num! (nth args 1))
                     price  (f/num! (nth args 2))
                     days   (days-between-serial settle mat)]
                 (when (or (<= price 0.0) (<= days 0.0) (> days 365.0))
                   (f/domain-error! :num))
                 (val/number (* (/ (- 100.0 price) price)
                                (/ 360.0 days)))))
             :arity [3 3])

(f/register! "RECEIVED"
             ^{:scalar? true}
             (fn [args]
               (let [settle (f/num! (nth args 0))
                     mat    (f/num! (nth args 1))
                     inv    (f/num! (nth args 2))
                     disc   (f/num! (nth args 3))
                     days   (days-between-serial settle mat)]
                 (when (or (<= inv 0.0) (<= disc 0.0) (<= days 0.0))
                   (f/domain-error! :num))
                 (val/number (/ inv (- 1.0 (* disc (/ days 360.0)))))))
             :arity [4 5])

(f/register! "DISC"
             ^{:scalar? true}
             (fn [args]
               (let [settle (f/num! (nth args 0))
                     mat    (f/num! (nth args 1))
                     price  (f/num! (nth args 2))
                     redeem (f/num! (nth args 3))
                     days   (days-between-serial settle mat)]
                 (when (or (<= price 0.0) (<= redeem 0.0) (<= days 0.0))
                   (f/domain-error! :num))
                 (val/number (* (- 1.0 (/ price redeem)) (/ 360.0 days)))))
             :arity [4 5])

(f/register! "INTRATE"
             ^{:scalar? true}
             (fn [args]
               (let [settle (f/num! (nth args 0))
                     mat    (f/num! (nth args 1))
                     inv    (f/num! (nth args 2))
                     redeem (f/num! (nth args 3))
                     days   (days-between-serial settle mat)]
                 (when (or (<= inv 0.0) (<= redeem 0.0) (<= days 0.0))
                   (f/domain-error! :num))
                 (val/number (* (/ (- redeem inv) inv) (/ 360.0 days)))))
             :arity [4 5])

(f/register! "ACCRINTM"
  ;; Accrued interest on a maturity-paying security
             ^{:scalar? true}
             (fn [args]
               (let [issue  (f/num! (nth args 0))
                     settle (f/num! (nth args 1))
                     rate   (f/num! (nth args 2))
                     par    (f/num! (nth args 3))
                     days   (days-between-serial issue settle)]
                 (when (or (<= rate 0.0) (<= par 0.0) (<= days 0.0))
                   (f/domain-error! :num))
                 (val/number (* par rate (/ days 360.0)))))
             :arity [4 5])

;; ---------------------------------------------------------------------------
;; Bond / coupon math — complex day-count-dependent; POI leaves these
;; NotImplemented. We register as #N/A stubs for parity.

(defn- na-stub [fname arity]
  (f/register! fname (fn [_args] val/ERR-NA) :arity arity))

(doseq [[nm arity] [["ACCRINT"   [6 8]]
                    ["AMORDEGRC" [6 7]]
                    ["AMORLINC"  [6 7]]
                    ["COUPDAYBS" [3 4]]
                    ["COUPDAYS"  [3 4]]
                    ["COUPDAYSNC" [3 4]]
                    ["COUPNCD"   [3 4]]
                    ["COUPNUM"   [3 4]]
                    ["COUPPCD"   [3 4]]
                    ["DURATION"  [5 6]]
                    ["MDURATION" [5 6]]
                    ["PRICE"     [6 7]]
                    ["PRICEDISC" [4 5]]
                    ["PRICEMAT"  [5 6]]
                    ["YIELD"     [6 7]]
                    ["YIELDDISC" [4 5]]
                    ["YIELDMAT"  [5 6]]]]
  (na-stub nm arity))
