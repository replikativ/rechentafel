(ns rechentafel.rng-test
  "Verify seedable RNG behaviour: same seed → same sequence, both
  within a single recalc and across forks of the same wb. Runs on
  JVM and cljs."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.rng :as rng]
            [rechentafel.functions.all]))

(defn- mk [& cells]
  (reduce (fn [wb [r col input]]
            (e/set-cell wb (c/pack 0 r col) input))
          (e/empty-workbook)
          cells))

;; ---------------------------------------------------------------------------
;; Direct PRNG smoke

(deftest seeded-rng-deterministic
  (let [r1 (rng/make 42)
        r2 (rng/make 42)
        s1 (vec (repeatedly 5 r1))
        s2 (vec (repeatedly 5 r2))]
    (is (= s1 s2) "same seed → same sequence")
    (is (every? #(and (>= % 0.0) (< % 1.0)) s1)
        "every draw is in [0,1)")))

(deftest different-seeds-diverge
  (let [r1 (rng/make 42)
        r2 (rng/make 43)]
    (is (not= [(r1) (r1) (r1)] [(r2) (r2) (r2)]))))

;; ---------------------------------------------------------------------------
;; RAND-family fns inside recalc

(deftest seeded-recalc-rand-reproducible
  (let [wb (-> (mk [0 0 "=RAND()"]
                   [0 1 "=RAND()"]
                   [0 2 "=RAND()"])
               (assoc :rng-seed 1234))
        a  (e/recalc wb)
        b  (e/recalc wb)]
    (is (= (:v (e/get-cell a (c/pack 0 0 0)))
           (:v (e/get-cell b (c/pack 0 0 0)))))
    (is (= (:v (e/get-cell a (c/pack 0 0 1)))
           (:v (e/get-cell b (c/pack 0 0 1)))))
    (is (= (:v (e/get-cell a (c/pack 0 0 2)))
           (:v (e/get-cell b (c/pack 0 0 2)))))))

(deftest seeded-randbetween-reproducible
  (let [wb (-> (mk [0 0 "=RANDBETWEEN(1, 100)"])
               (assoc :rng-seed 7))
        a  (:v (e/get-cell (e/recalc wb) (c/pack 0 0 0)))
        b  (:v (e/get-cell (e/recalc wb) (c/pack 0 0 0)))]
    (is (= a b))
    (is (and (>= a 1) (<= a 100)))))

(deftest seeded-randarray-reproducible
  (let [wb (-> (mk [0 0 "=RANDARRAY(5)"])
               (assoc :rng-seed 99))
        a  (e/recalc wb)
        b  (e/recalc wb)]
    (doseq [i (range 5)]
      (is (= (:v (e/get-cell a (c/pack 0 i 0)))
             (:v (e/get-cell b (c/pack 0 i 0))))
          (str "row " i)))))

(deftest different-seeds-give-different-results
  (let [wb (mk [0 0 "=RAND()"])
        a  (:v (e/get-cell (e/recalc (assoc wb :rng-seed 1)) (c/pack 0 0 0)))
        b  (:v (e/get-cell (e/recalc (assoc wb :rng-seed 2)) (c/pack 0 0 0)))]
    (is (not= a b))))

(deftest unseeded-still-works
  ;; Regression: un-seeded wb falls back to clojure.core/rand.
  (let [wb (e/recalc (mk [0 0 "=RAND()"]))
        v  (:v (e/get-cell wb (c/pack 0 0 0)))]
    (is (and (>= v 0.0) (< v 1.0)))))

;; ---------------------------------------------------------------------------
;; Fork-and-replay — the actual usage pattern

(deftest fork-from-same-seed-produces-same-result
  ;; Take a base wb, add the RAND-bearing formula in two parallel
  ;; "branches", recalc each. Same seed → same result, no shared
  ;; mutable state.
  (let [base (-> (e/empty-workbook)
                 (e/set-cell (c/pack 0 0 0) "=RAND()*100")
                 (assoc :rng-seed 555))
        b1   (e/recalc base)
        b2   (e/recalc base)]
    (is (= (:v (e/get-cell b1 (c/pack 0 0 0)))
           (:v (e/get-cell b2 (c/pack 0 0 0)))))))
