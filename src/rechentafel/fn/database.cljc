(ns rechentafel.fn.database
  "Database functions (POI category: database — 12 fns).

  All 12 share the DStarRunner shape: DSUM(db, field, criteria).

    db       — area whose first row is a header of column names
    field    — number (1-based) or string (column name) selecting the
               aggregation column
    criteria — area whose first row is a header (subset of db's headers)
               and subsequent rows are match specs. Rows are OR'd; cells
               within a row are AND'd. Cell contents may be a literal
               (exact equality) or a comparison string like `>5`, `<>x`,
               `=foo`.

  Each fn picks different aggregation over the filtered field column."
  (:require [clojure.string :as str]
            [rechentafel.platform :as p]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Parsing the criteria predicates

(defn- criteria-pred
  "Given a criteria cell, return a predicate (val -> bool)."
  [crit]
  (cond
    (val/blank? crit) (constantly true)
    (val/num? crit)
    (fn [v] (and (val/num? v) (== (:v v) (:v crit))))
    (val/bool? crit)
    (fn [v] (and (val/bool? v) (= (:v v) (:v crit))))
    (val/str? crit)
    (let [s (:v crit)
          [op rest]
          (cond
            (str/starts-with? s ">=") [:>= (subs s 2)]
            (str/starts-with? s "<=") [:<= (subs s 2)]
            (str/starts-with? s "<>") [:<> (subs s 2)]
            (str/starts-with? s ">")  [:> (subs s 1)]
            (str/starts-with? s "<")  [:< (subs s 1)]
            (str/starts-with? s "=")  [:= (subs s 1)]
            :else                     [:eq s])
          parsed (p/parse-double rest)
          num-cmp (fn [pred]
                    (fn [v]
                      (let [n (val/to-num v)]
                        (and (val/num? n) parsed
                             (pred (double (:v n)) (double parsed))))))]
      (case op
        :>= (num-cmp >=)
        :<= (num-cmp <=)
        :>  (num-cmp >)
        :<  (num-cmp <)
        :<> (fn [v] (not= (some-> v val/to-str :v) rest))
        :=  (fn [v] (= (some-> v val/to-str :v str/lower-case)
                       (some-> rest str/lower-case)))
        :eq (fn [v] (= (some-> v val/to-str :v str/lower-case)
                       (str/lower-case s)))))
    :else (constantly false)))

;; ---------------------------------------------------------------------------
;; Header map + criteria matching

(defn- header-index [rows]
  (into {} (map-indexed (fn [i c]
                          [(some-> (:v (val/to-str c)) str/lower-case) i])
                        (first rows))))

(defn- matches-criteria?
  "Row from db matches criteria if any criteria row matches. A criteria
  row matches if every non-blank cell's column passes its predicate."
  [db-header db-row crit-rows]
  (let [crit-header (first crit-rows)]
    (some (fn [crit-row]
            (every?
             true?
             (map-indexed
              (fn [i crit-cell]
                (if (val/blank? crit-cell)
                  true
                  (let [col-name (some-> (:v (val/to-str (nth crit-header i)))
                                         str/lower-case)
                        db-col-idx (get db-header col-name)]
                    (if db-col-idx
                      ((criteria-pred crit-cell) (nth db-row db-col-idx))
                      false))))
              crit-row)))
          (rest crit-rows))))

;; ---------------------------------------------------------------------------
;; Common dstar-runner

(defn- db-rows [v]
  (if (val/area? v) (:values v) [[v]]))

(defn- resolve-field [db-header field]
  (cond
    (val/num? field)  (let [i (dec (long (:v field)))]
                        (when (neg? i) (f/domain-error! :value))
                        i)
    (val/str? field)  (or (get db-header (str/lower-case (:v field)))
                          (f/domain-error! :value))
    :else (f/domain-error! :value)))

(defn- filtered-column
  "Return a vector of selected-field values from db rows passing criteria."
  [db-area field crit-area]
  (let [db (db-rows db-area)
        header (header-index db)
        field-idx (resolve-field header field)
        crit (db-rows crit-area)
        out  (volatile! (transient []))]
    (doseq [row (rest db)]
      (when (matches-criteria? header row crit)
        (vswap! out conj! (nth row field-idx))))
    (persistent! @out)))

(defn- numeric-values [cells]
  (reduce (fn [acc c]
            (cond
              (val/err? c) (f/domain-error! (:v c))
              (val/num? c) (conj acc (double (:v c)))
              :else acc))
          [] cells))

(defn- reg-dstar [name reducer]
  (f/register! name
               (fn [args]
                 (let [cells (filtered-column (nth args 0) (nth args 1) (nth args 2))]
                   (reducer cells)))
               :arity [3 3]))

(reg-dstar "DSUM"
           (fn [cells]
             (val/number (reduce + 0.0 (numeric-values cells)))))

(reg-dstar "DAVERAGE"
           (fn [cells]
             (let [ns (numeric-values cells)]
               (if (empty? ns) val/ERR-DIV0
                   (val/number (/ (reduce + 0.0 ns) (double (count ns))))))))

(reg-dstar "DCOUNT"
           (fn [cells]
             (val/number (double (count (numeric-values cells))))))

(reg-dstar "DCOUNTA"
           (fn [cells]
             (val/number (double (count (remove val/blank? cells))))))

(reg-dstar "DMAX"
           (fn [cells]
             (let [ns (numeric-values cells)]
               (if (empty? ns) (val/number 0.0)
                   (val/number (apply max ns))))))

(reg-dstar "DMIN"
           (fn [cells]
             (let [ns (numeric-values cells)]
               (if (empty? ns) (val/number 0.0)
                   (val/number (apply min ns))))))

(reg-dstar "DPRODUCT"
           (fn [cells]
             (let [ns (numeric-values cells)]
               (val/number (reduce * 1.0 ns)))))

(reg-dstar "DGET"
           (fn [cells]
             (cond
               (empty? cells)       val/ERR-VALUE
               (= 1 (count cells))  (first cells)
               :else                val/ERR-NUM)))

(defn- variance [ns sample?]
  (let [n (count ns)]
    (if (or (empty? ns) (and sample? (<= n 1)))
      nil
      (let [mean (/ (reduce + 0.0 ns) (double n))
            sq   (reduce + 0.0 (map #(let [d (- % mean)] (* d d)) ns))
            denom (if sample? (dec n) n)]
        (/ sq (double denom))))))

(reg-dstar "DVAR"
           (fn [cells]
             (if-let [v (variance (numeric-values cells) true)]
               (val/number v) val/ERR-DIV0)))

(reg-dstar "DVARP"
           (fn [cells]
             (if-let [v (variance (numeric-values cells) false)]
               (val/number v) val/ERR-DIV0)))

(reg-dstar "DSTDEV"
           (fn [cells]
             (if-let [v (variance (numeric-values cells) true)]
               (val/number (Math/sqrt v)) val/ERR-DIV0)))

(reg-dstar "DSTDEVP"
           (fn [cells]
             (if-let [v (variance (numeric-values cells) false)]
               (val/number (Math/sqrt v)) val/ERR-DIV0)))
