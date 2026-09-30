(ns rechentafel.filterxml-test
  "FILTERXML: strict XML, XPath 1.0 without namespaces, and Excel's
   results (one node a value, several a column, numeric text a number)."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.fn.xml :as x]
            [rechentafel.functions.all]))

(defn- run [cells f]
  (let [wb (reduce (fn [wb [r col v]] (e/set-cell wb (c/pack 0 r col) v)) (e/empty-workbook) cells)]
    (:v (e/get-cell (e/recalc (e/set-cell wb (c/pack 0 9 9) f)) (c/pack 0 9 9)))))

(defn- col [v] (mapv (comp :v first) (:values v)))

(def ^:private doc "<r><s n=\"1\">a</s><s n=\"2\">b</s><s>7</s><t>x<u>y</u></t></r>")

(deftest selects-nodes
  (is (= ["a" "b" 7.0] (col (x/filter-xml doc "//s"))) "numeric text is a number")
  (is (= {:t :str :v "a"} (x/filter-xml doc "//s[1]")) "one node is a value")
  (is (= {:t :num :v 7.0} (x/filter-xml doc "//s[last()]")))
  (is (= ["b" 7.0] (col (x/filter-xml doc "//s[position()>1]"))))
  (is (= [1.0 2.0] (col (x/filter-xml doc "//s/@n"))))
  (is (= {:t :str :v "b"} (x/filter-xml doc "//s[@n='2']")))
  (is (= {:t :str :v "xy"} (x/filter-xml doc "/r/t")) "an element's text is all its text")
  (is (= {:t :str :v "x"} (x/filter-xml doc "//t/text()")))
  (is (= ["a" "b" 7.0 "xy"] (col (x/filter-xml doc "/r/*"))))
  (is (= {:t :num :v 7.0} (x/filter-xml doc "//s[.*0=0]")) "the numeric ones")
  (is (= {:t :str :v "y"} (x/filter-xml doc "//u[ancestor::t]")))
  (is (= ["b" 7.0] (col (x/filter-xml doc "//s[preceding-sibling::s]"))))
  (is (= {:t :str :v "a"} (x/filter-xml doc "//s[following-sibling::s[2]]")))
  (is (= ["a" "xy"] (col (x/filter-xml doc "//s[1] | //t")))))

(deftest expressions
  (is (= {:t :num :v 3.0} (x/filter-xml doc "count(//s)")))
  (is (= {:t :num :v 1.0} (x/filter-xml doc "//s[1]='a'")) "a boolean is 1 or 0, as LibreOffice")
  (is (= {:t :str :v "ab"} (x/filter-xml doc "concat(//s[1],//s[2])")))
  (is (= {:t :num :v 10.0} (x/filter-xml "<a><b>3</b><b>7</b></a>" "sum(//b)")))
  (is (= ["a" "c"] (col (x/filter-xml "<a><b>a</b><b>c</b><b>a</b></a>" "//b[not(.=preceding::b)]")))
      "the unique ones")
  (is (= {:t :str :v "c"} (x/filter-xml "<a><b>a</b><b>c</b><b>a</b></a>" "//b[not(.=preceding::b)][2]")))
  (is (= {:t :num :v 2.0} (x/filter-xml doc "7 mod 5")))
  (is (= {:t :str :v "b"} (x/filter-xml doc "substring('abc',2,1)"))))

(deftest what-excel-cannot-read-is-value
  (doseq [[xml xp] [["<a><b>1</a>" "//b"] ["<a>&x;</a>" "//a"] ["<!DOCTYPE a [<!ENTITY x \"y\">]><a>&x;</a>" "//a"]
                    ["<a/><b/>" "//a"] ["<a>1</a>" "//["] ["<a>1</a>" "//z"] ["text" "//a"]]]
    (is (= {:t :err :v :value} (x/filter-xml xml xp)) (str xml " " xp))))

(deftest entities-and-cdata
  (is (= {:t :str :v "a&b<c"} (x/filter-xml "<a>a&amp;b&lt;c</a>" "//a")))
  (is (= {:t :str :v "é"} (x/filter-xml "<a>&#233;</a>" "//a")))
  (is (= {:t :str :v "<raw>"} (x/filter-xml "<?xml version=\"1.0\"?><!-- c --><a><![CDATA[<raw>]]></a>" "//a"))))

(deftest splitting-text-in-a-workbook
  (is (= 3.0 (run [[0 0 "a, b, c"]] "=COUNTA(FILTERXML(\"<k><m>\"&SUBSTITUTE(A1,\", \",\"</m><m>\")&\"</m></k>\",\"//m\"))")))
  (is (= "b" (run [[0 0 "a, b, c"]] "=INDEX(_xlfn.FILTERXML(\"<k><m>\"&SUBSTITUTE(A1,\", \",\"</m><m>\")&\"</m></k>\",\"//m\"),2)")))
  (is (= 6.0 (run [[0 0 "1-2-3"]] "=SUM(FILTERXML(\"<k><m>\"&SUBSTITUTE(A1,\"-\",\"</m><m>\")&\"</m></k>\",\"//m\"))")))
  (is (= 5.0 (run [[0 0 "x"] [1 0 "y"] [0 1 2] [1 1 3] [0 2 "x, y"]]
                  "=SUM(SUMIFS(B1:B2,A1:A2,FILTERXML(\"<k><m>\"&SUBSTITUTE(C1,\", \",\"</m><m>\")&\"</m></k>\",\"//m\")))"))
      "criteria from FILTERXML, as SpreadsheetBench 54144"))
