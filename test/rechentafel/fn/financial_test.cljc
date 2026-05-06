(ns rechentafel.fn.financial-test
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.financial]))

(defn- n [x] (v/number x))

(defn- area [rows]
  {:t :area :r0 0 :c0 0
   :r1 (dec (count rows)) :c1 (dec (count (first rows)))
   :values (mapv (fn [row] (mapv #(n %) row)) rows)})

(defn- approx [expected actual tol]
  (< (Math/abs (- (double expected) (double (:v actual))))
     tol))

(deftest annuity-core
  ;; $200K mortgage at 6%/yr for 30 yrs (monthly) → PMT ≈ -1199.10
  (is (approx -1199.101 (f/call "PMT" [(n (/ 0.06 12)) (n 360) (n 200000)]) 1e-2))
  ;; Invest $100/mo at 5%/yr for 10 yr → FV ≈ 15528.23
  (is (approx 15528.23 (f/call "FV" [(n (/ 0.05 12)) (n 120) (n -100)]) 1e-1))
  ;; PV of $100k in 10 yrs at 5% → -61391.33
  (is (approx -61391.33 (f/call "PV" [(n 0.05) (n 10) (n 0) (n 100000)]) 1e-1))
  ;; RATE: 30-yr monthly loan, payment -1199.10, pv 200000 → 0.005
  (is (approx 0.005 (f/call "RATE" [(n 360) (n -1199.10) (n 200000)]) 1e-4)))

(deftest interest-principal
  ;; First-period interest on 30-yr $200K at 6% is exactly -1000
  (is (approx -1000.0  (f/call "IPMT" [(n (/ 0.06 12)) (n 1) (n 360) (n 200000)]) 1e-6))
  (is (approx -199.101 (f/call "PPMT" [(n (/ 0.06 12)) (n 1) (n 360) (n 200000)]) 1e-2))
  ;; ISPMT(r, per, nper, pv) = pv * r * (per/nper - 1)
  ;; = 10000 * 0.1 * (1/10 - 1) = 10000 * 0.1 * -0.9 = -900
  (is (approx -900.0 (f/call "ISPMT" [(n 0.1) (n 1) (n 10) (n 10000)]) 1e-6))
  ;; CUMIPMT(r=0.005, n=360, pv=200000, 1, 12, 0): interest paid in yr 1
  (is (approx -11933.0 (f/call "CUMIPMT"
                               [(n (/ 0.06 12)) (n 360) (n 200000)
                                (n 1) (n 12) (n 0)])
              2.0)))

(deftest cash-flows
  ;; IRR of [-1000, 300, 400, 500, 200] ≈ 0.153
  (is (approx 0.1532 (f/call "IRR"
                             [(area [[-1000 300 400 500 200]])])
              1e-3))
  ;; NPV at 10%: 300/1.1 + 400/1.21 + 500/1.331 + 200/1.4641 ≈ 1105.06
  ;; then +(-1000) → 105.06 (Excel convention: NPV doesn't include v0 at t=0)
  (is (approx 105.06 (f/call "NPV" [(n 0.1)
                                    (area [[-1000 300 400 500 200]])])
              1e-1))
  ;; MIRR: finance=10%, reinvest=12%
  (is (number? (:v (f/call "MIRR"
                           [(area [[-1000 300 400 500 200]])
                            (n 0.1) (n 0.12)])))))

(deftest depreciation
  ;; SLN: (30000-7500)/10 = 2250
  (is (approx 2250.0 (f/call "SLN" [(n 30000) (n 7500) (n 10)]) 1e-9))
  ;; SYD: year 1 = (30000-7500)*10 / 55 = 4090.909
  (is (approx 4090.909 (f/call "SYD" [(n 30000) (n 7500) (n 10) (n 1)]) 1e-2))
  ;; DDB: first period of 2400/300/10-yr = 2400 * 2/10 = 480
  (is (approx 480.0 (f/call "DDB" [(n 2400) (n 300) (n 10) (n 1)]) 1e-9)))

(deftest rate-conversion
  ;; EFFECT(0.0525, 4) = (1.013125)^4 - 1 ≈ 0.053543
  (is (approx 0.053543 (f/call "EFFECT"  [(n 0.0525) (n 4)]) 1e-5))
  (is (approx 0.052459 (f/call "NOMINAL" [(n 0.0535) (n 4)]) 1e-5))
  ;; Round-trip: NOMINAL(EFFECT(r,m), m) = r
  (let [r 0.07
        e (:v (f/call "EFFECT"  [(n r) (n 12)]))
        r' (:v (f/call "NOMINAL" [(n e) (n 12)]))]
    (is (< (Math/abs (- r r')) 1e-9))))

(deftest tbill
  ;; 91-day T-bill at 5% discount
  (is (approx 98.736 (f/call "TBILLPRICE"
                             [(n 0) (n 91) (n 0.05)]) 1e-2))
  ;; TBILLEQ / TBILLYIELD return rates (not percentages)
  (is (approx 0.05134 (f/call "TBILLEQ"
                              [(n 0) (n 91) (n 0.05)]) 1e-4))
  (is (approx 0.05065 (f/call "TBILLYIELD"
                              [(n 0) (n 91) (n 98.736)]) 1e-4)))

(deftest not-implemented-stubs
  ;; Bond/coupon fns are #N/A stubs
  (is (= v/ERR-NA (f/call "PRICE" [(n 0) (n 365) (n 0.05) (n 0.06) (n 100) (n 2)])))
  (is (= v/ERR-NA (f/call "YIELD" [(n 0) (n 365) (n 0.05) (n 98) (n 100) (n 2)])))
  (is (= v/ERR-NA (f/call "COUPNUM" [(n 0) (n 730) (n 2)]))))
