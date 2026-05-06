(ns rechentafel.oracle.fn.database-poi-test
  "Cross-check database DXX functions against POI."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))

(defn- area [rows]
  (let [wrap (fn [x]
               (cond (number? x)  (n x)
                     (string? x)  (s x)
                     :else        x))]
    {:t :area
     :values (mapv #(mapv wrap %) rows)
     :r0 0 :c0 0
     :r1 (dec (count rows)) :c1 (dec (count (first rows)))}))

(defn- check! [cases]
  (let [{:keys [failures] :as r} (po/check-many cases)]
    (doseq [fail failures] (is false (po/report fail)))
    (is (zero? (count failures))
        (str "passed " (:passed r) "/" (:total r)))))

(def ^:private db-values
  [["Name"  "Age" "Dept"  "Salary"]
   ["Alice"  30   "Eng"   90000]
   ["Bob"    45   "Sales" 70000]
   ["Carol"  35   "Eng"   110000]
   ["Dan"    28   "Sales" 60000]
   ["Eve"    50   "Eng"   120000]])

(def ^:private db (area db-values))

;; Criteria: Dept = "Eng"
(def ^:private crit-eng
  (area [["Dept"] ["Eng"]]))

;; Criteria: Age > 30
(def ^:private crit-over-30
  (area [["Age"] [">30"]]))

(deftest dxx-simple
  (check! [;; Salaries of Engineering employees: 90k + 110k + 120k = 320k
           {:fname "DSUM"     :args [db (s "Salary") crit-eng]}
           {:fname "DAVERAGE" :args [db (s "Salary") crit-eng]}
           {:fname "DCOUNT"   :args [db (s "Salary") crit-eng]}
           {:fname "DCOUNTA"  :args [db (s "Name")   crit-eng]}
           {:fname "DMAX"     :args [db (s "Salary") crit-eng]}
           {:fname "DMIN"     :args [db (s "Salary") crit-eng]}
           {:fname "DPRODUCT" :args [db (s "Age")    crit-eng]}
           {:fname "DSTDEV"   :args [db (s "Salary") crit-eng]}
           {:fname "DSTDEVP"  :args [db (s "Salary") crit-eng]}
           {:fname "DVAR"     :args [db (s "Salary") crit-eng]}
           {:fname "DVARP"    :args [db (s "Salary") crit-eng]}]))

(deftest dxx-comparison
  (check! [{:fname "DSUM"     :args [db (s "Salary") crit-over-30]}
           {:fname "DAVERAGE" :args [db (s "Age")    crit-over-30]}
           {:fname "DMAX"     :args [db (s "Salary") crit-over-30]}
           {:fname "DMIN"     :args [db (s "Salary") crit-over-30]}]))

(deftest dget
  ;; DGET returns the single matching value, or #NUM!/#VALUE! if zero/many.
  ;; Criteria matching only Carol (Dept=Eng AND Age=35)
  (let [crit (area [["Name"] ["Carol"]])]
    (check! [{:fname "DGET" :args [db (s "Salary") crit]}])))
