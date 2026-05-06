(ns rechentafel.oracle.fn.info-poi-test
  "Cross-check info / predicate functions against POI."
  (:require [clojure.test :refer [deftest is]]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]
            [rechentafel.oracle.poi-oracle :as po]))

(defn- n [x] (v/number x))
(defn- s [x] (v/string x))
(defn- area [rows]
  {:t :area :values (mapv #(mapv v/number %) rows)
   :r0 0 :c0 0 :r1 (dec (count rows)) :c1 (dec (count (first rows)))})

(defn- check! [cases]
  (let [{:keys [failures] :as r} (po/check-many cases)]
    (doseq [fail failures] (is false (po/report fail)))
    (is (zero? (count failures))
        (str "passed " (:passed r) "/" (:total r)))))

(deftest predicates
  (check! [{:fname "ISNUMBER"  :args [(n 3)]}
           {:fname "ISNUMBER"  :args [(s "x")]}
           {:fname "ISTEXT"    :args [(s "hi")]}
           {:fname "ISTEXT"    :args [(n 1)]}
           {:fname "ISNONTEXT" :args [(s "hi")]}
           {:fname "ISNONTEXT" :args [(n 1)]}
           {:fname "ISLOGICAL" :args [v/TRUE]}
           {:fname "ISLOGICAL" :args [(n 1)]}
           {:fname "ISERROR"   :args [v/ERR-NA]}
           {:fname "ISERROR"   :args [v/ERR-DIV0]}
           {:fname "ISERROR"   :args [(n 0)]}
           {:fname "ISERR"     :args [v/ERR-NA]}       ;; #N/A excluded
           {:fname "ISERR"     :args [v/ERR-DIV0]}
           {:fname "ISNA"      :args [v/ERR-NA]}
           {:fname "ISNA"      :args [v/ERR-DIV0]}
           {:fname "ISEVEN"    :args [(n 4)]}
           {:fname "ISEVEN"    :args [(n 5)]}
           {:fname "ISODD"     :args [(n 5)]}
           {:fname "ISODD"     :args [(n 6)]}]))

(deftest na-and-error-type
  (check! [{:fname "NA"         :args []}
           {:fname "ERROR.TYPE" :args [v/ERR-NULL]}
           {:fname "ERROR.TYPE" :args [v/ERR-DIV0]}
           {:fname "ERROR.TYPE" :args [v/ERR-VALUE]}
           {:fname "ERROR.TYPE" :args [v/ERR-REF]}
           {:fname "ERROR.TYPE" :args [v/ERR-NAME]}
           {:fname "ERROR.TYPE" :args [v/ERR-NUM]}
           {:fname "ERROR.TYPE" :args [v/ERR-NA]}
           {:fname "ERROR.TYPE" :args [(n 5)]}]))       ;; returns #N/A

(deftest type-classifier
  (check! [{:fname "TYPE" :args [(n 1)]}               ;; 1
           {:fname "TYPE" :args [(s "x")]}             ;; 2
           {:fname "TYPE" :args [v/TRUE]}              ;; 4
           {:fname "TYPE" :args [v/ERR-DIV0]}          ;; 16
           {:fname "TYPE" :args [(area [[1 2] [3 4]])]}])) ;; 64

(deftest rows-columns
  (check! [{:fname "ROWS"    :args [(area [[1] [2] [3]])]}
           {:fname "ROWS"    :args [(area [[1 2 3]])]}
           {:fname "COLUMNS" :args [(area [[1 2 3]])]}
           {:fname "COLUMNS" :args [(area [[1] [2] [3]])]}]))
