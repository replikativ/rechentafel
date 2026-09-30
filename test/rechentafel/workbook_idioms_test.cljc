(ns rechentafel.workbook-idioms-test
  "Formula idioms from real workbooks (SpreadsheetBench), each as Excel
   computes it: AGGREGATE ignoring errors, INDEX as a reference, OFFSET's
   defaults, array lifting of IS*/MATCH/IFERROR/SUMIFS, number formats in
   TEXT, date and time text, TEXTSPLIT, and the storage prefixes of LET."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all]))

(defn- run
  "The value of formula `f` (in J10) over literal `cells` [[r c v] …]."
  [cells f]
  (let [wb (reduce (fn [wb [r col v]] (e/set-cell wb (c/pack 0 r col) v)) (e/empty-workbook) cells)]
    (:v (e/get-cell (e/recalc (e/set-cell wb (c/pack 0 9 9) f)) (c/pack 0 9 9)))))

(defn- near? [a b] (and (number? a) (< (Math/abs (- (double a) (double b))) 1e-9)))

(def ^:private col-a [[0 0 "x"] [1 0 "Lighting"] [2 0 "y"] [3 0 "Lighting"]])
(def ^:private grid [[0 0 1] [1 0 2] [2 0 3] [0 1 10] [1 1 20] [2 1 30]])

(deftest aggregate-ignores-errors-with-its-option
  (is (near? 4 (run col-a "=AGGREGATE(15,6,ROW(A1:A4)/(A1:A4=\"Lighting\"),2)")) "the k-th match")
  (is (near? 4 (run col-a "=AGGREGATE(14,6,ROW(A1:A4)/(A1:A4=\"Lighting\"),1)")))
  (is (= :num (run col-a "=AGGREGATE(15,6,ROW(A1:A4)/(A1:A4=\"Lighting\"),3)")) "no third match")
  (is (near? 5 (run [[0 0 1] [1 0 "=1/0"] [2 0 4]] "=AGGREGATE(9,6,A1:A3)")))
  (is (= :div0 (run [[0 0 1] [1 0 "=1/0"]] "=AGGREGATE(9,4,A1:A2)")) "option 4 ignores nothing"))

(deftest a-formula-shows-an-empty-cell-as-zero
  (is (near? 0 (run [] "=A1")))
  (is (near? 0 (run [[0 0 1]] "=INDEX(A1:A3,2)")))
  (is (= "" (run [] "=\"\"")) "an empty string stays text"))

(deftest index-is-a-reference
  (is (near? 3 (run grid "=SUM(A1:INDEX(A1:A3,2))")))
  (is (near? 5 (run grid "=SUM(INDEX(A1:A3,2):INDEX(A1:A3,3))")))
  (is (near? 33 (run grid "=SUM(A1:INDEX(A1:B3,2,2))")))
  (is (near? 31 (run grid "=INDEX(A1:B3,3,2)+1")))
  (is (near? 2 (run grid "=COUNTIFS(A1:INDEX(A1:A3,3),\">1\")"))))

(deftest offset-defaults-and-computed-bases
  (let [g [[0 0 1] [0 1 2] [0 2 3] [1 0 4] [1 1 5] [1 2 6]]]
    (is (near? 6 (run g "=SUM(OFFSET(A1,0,0,,3))")) "an omitted height is the base's")
    (is (near? 7 (run g "=SUM(OFFSET(A1:A2,0,1,2,))")) "an omitted width is the base's")
    (is (near? 12 (run g "=SUM(OFFSET(INDIRECT(\"A1\"),,,2,2))")))
    (is (near? 5 (run g "=SUM(OFFSET(INDEX(A1:C2,1,2),1,0))")))))

(deftest predicates-and-match-lift-over-arrays
  (is (near? 2 (run col-a "=SUMPRODUCT(--ISNUMBER(SEARCH(\"light\",A1:A4)))")))
  (is (near? 2 (run col-a "=SUMPRODUCT(--ISERROR(SEARCH(\"light\",A1:A4)))")))
  (is (near? 2 (run col-a "=SUMPRODUCT(--ISNUMBER(MATCH(A1:A4,{\"Lighting\"},0)))")))
  (is (true? (run col-a "=ISREF(A1:A4)"))))

(deftest approximate-match-is-a-binary-search
  (let [asc [[0 0 1] [1 0 3] [2 0 5] [3 0 7]]]
    (is (near? 3 (run asc "=MATCH(6,A1:A4)")))
    (is (near? 4 (run asc "=MATCH(7,A1:A4,1)")))
    (is (= :na (run asc "=MATCH(0,A1:A4)")))
    (is (near? 2 (run [[0 0 7] [1 0 5] [2 0 3]] "=MATCH(4,A1:A3,-1)")))))

(deftest iferror-replaces-errors-in-an-array
  (is (near? 0.5 (run [[0 0 0] [0 1 2]] "=SUM(IFERROR(1/A1:B1,\"\"))")))
  (is (near? 2 (run [] "=SUM(IFERROR({1,2}/{0,1},0))"))))

(deftest criteria-arrays-in-the-ifs-family
  (let [d [[0 0 "a"] [1 0 "b"] [2 0 "a"] [0 1 1] [1 1 2] [2 1 4]]]
    (is (near? 7 (run d "=SUMPRODUCT(SUMIFS(B1:B3,A1:A3,{\"a\",\"b\"}))")))
    (is (near? 7 (run d "=SUM(SUMIF(A1:A3,{\"a\",\"b\"},B1:B3))")))
    (is (near? 6 (run d "=SUM(MAXIFS(B1:B3,A1:A3,{\"a\";\"b\"}))")))))

(deftest text-formats
  (doseq [[f want] [["=TEXT(10,\"000\")" "010"] ["=TEXT(1234.567,\"#,##0.00\")" "1,234.57"]
                    ["=TEXT(2.675,\"0.00\")" "2.68"] ["=TEXT(-5,\"0;(0)\")" "(5)"] ["=TEXT(-5,\"0\")" "-5"]
                    ["=TEXT(0,\"0;-0;\"\"zero\"\"\")" "zero"] ["=TEXT(0.256,\"0.0%\")" "25.6%"]
                    ["=TEXT(1500000,\"0.0,,\"\"M\"\"\")" "1.5M"] ["=TEXT(5551234,\"000-0000\")" "555-1234"]
                    ["=TEXT(1.5,\"0.0#\")" "1.5"] ["=TEXT(12345,\"0.00E+00\")" "1.23E+04"]
                    ["=TEXT(4,\"[<3]\"\"low\"\"0;\")" ""] ["=TEXT(2,\"[<3]\"\"cat\"\"0;\")" "cat2"]
                    ["=TEXT(\"abc\",\"0\")" "abc"] ["=TEXT(\"abc\",\"0;0;0;@\"\"!\"\"\")" "abc!"]
                    ["=TEXT(1234,\"$#,##0\")" "$1,234"] ["=TEXT(0.123,\"#.00\")" ".12"] ["=TEXT(7,\"General\")" "7"]]]
    (is (= want (run [] f)) f)))

(deftest date-and-time-text
  (is (near? 44501 (run [] "=EDATE(\"1Oct 21\",1)")))
  (is (near? 45322 (run [] "=EOMONTH(\"1Jan2024\",0)")))
  (is (near? 29 (run [] "=DAY(EOMONTH(1&\"Feb\"&2024,0))")))
  (is (near? (/ 22.0 24) (run [] "=\"22:30\"-TIME(0,30,0)")))
  (is (near? 44470 (run [] "=DATEVALUE(\"Oct 2021\")")))
  (is (near? 0.9375 (run [] "=TIMEVALUE(\"10:30 PM\")")))
  (is (near? 43831.5 (run [] "=\"1/1/2020 12:00\"+0")))
  (is (= :value (run [] "=\"Oct\"+0")) "a month alone is not a date"))

(deftest text-functions-from-files
  (is (= "a,b" (run [] "=TEXTJOIN(\",\",,{\"a\",\"\",\"b\"})")) "an empty ignore_empty ignores empties")
  (is (near? 3 (run [] "=COUNTA(_xlfn.TEXTSPLIT(\"a, b, c\",\", \"))")))
  (is (= "c" (run [] "=INDEX(TEXTSPLIT(\"a,b;c,d\",\",\",\";\"),2,1)")))
  (is (near? 6 (run [] "=_xlfn.LET(_xlpm.w,5,_xlpm.w+1)")) "LET as stored in a file")
  (is (near? 1 (run [] "=_xlfn.LET(_xlpm.t,_xlfn.TEXTSPLIT(\"xa, yb, zc\",\", \"),_xlpm.ft,_xlfn._xlws.FILTER(_xlpm.t,(LEFT(_xlpm.t,1)<>\"x\")*(LEFT(_xlpm.t,1)<>\"z\"),\"\"),SUM(IF(_xlpm.ft=\"\",0,1)))"))))
