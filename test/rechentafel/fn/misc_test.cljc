(ns rechentafel.fn.misc-test
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.fn.misc]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- area [rows]
  {:t :area :r0 0 :c0 0
   :r1 (dec (count rows)) :c1 (dec (count (first rows)))
   :values (mapv (fn [row] (mapv #(n %) row)) rows)})

(defn- approx [expected actual tol]
  (< (Math/abs (- (double expected) (double (:v actual)))) tol))

(deftest address-a1
  (is (= (s "$A$1")          (f/call "ADDRESS" [(n 1) (n 1)])))
  (is (= (s "$C$2")          (f/call "ADDRESS" [(n 2) (n 3)])))
  (is (= (s "C$2")           (f/call "ADDRESS" [(n 2) (n 3) (n 2)])))
  (is (= (s "$C2")           (f/call "ADDRESS" [(n 2) (n 3) (n 3)])))
  (is (= (s "C2")            (f/call "ADDRESS" [(n 2) (n 3) (n 4)])))
  (is (= (s "$AA$1")         (f/call "ADDRESS" [(n 1) (n 27)])))
  (is (= (s "$AB$1")         (f/call "ADDRESS" [(n 1) (n 28)])))
  (is (= (s "$ZZ$1")         (f/call "ADDRESS" [(n 1) (n 702)])))
  (is (= (s "Sheet1!$C$2")   (f/call "ADDRESS" [(n 2) (n 3) (n 1) v/TRUE (s "Sheet1")]))))

(deftest address-r1c1
  (is (= (s "R2C3")          (f/call "ADDRESS" [(n 2) (n 3) (n 1) v/FALSE])))
  (is (= (s "R2C[3]")        (f/call "ADDRESS" [(n 2) (n 3) (n 2) v/FALSE])))
  (is (= (s "R[2]C3")        (f/call "ADDRESS" [(n 2) (n 3) (n 3) v/FALSE])))
  (is (= (s "R[2]C[3]")      (f/call "ADDRESS" [(n 2) (n 3) (n 4) v/FALSE]))))

(deftest dollar-conv
  ;; DOLLARDE(1.02, 16) = 1 + 2/16 = 1.125
  (is (approx 1.125 (f/call "DOLLARDE" [(n 1.02)  (n 16)]) 1e-9))
  ;; DOLLARFR round-trip
  (is (approx 1.02  (f/call "DOLLARFR" [(n 1.125) (n 16)]) 1e-9)))

(deftest fvschedule
  ;; 1000 * 1.05 * 1.03 * 1.04
  (is (approx 1124.76 (f/call "FVSCHEDULE"
                              [(n 1000) (area [[0.05 0.03 0.04]])])
              1e-6)))

(deftest stubs
  (is (= v/ERR-NA (f/call "ABSREF"   [(n 1) (n 1)])))
  (is (= v/ERR-NA (f/call "BAHTTEXT" [(n 1)])))
  (is (= v/ERR-NA (f/call "CUBESET"  [(s "db") (s "set")])))
  (is (= v/ERR-NA (f/call "RTD"      [(s "id") (s "srv") (s "q")]))))
