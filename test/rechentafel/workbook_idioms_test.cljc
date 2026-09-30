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

(deftest arrays-in-if-and-lookups
  (is (near? 4 (run [[0 0 1] [1 0 "=1/0"] [2 0 3]] "=SUM(IF(ISERROR(A1:A3),\"\",A1:A3))"))
      "IF takes each element's branch; an error in the other one is not its result")
  (is (near? 3 (run [[0 0 "Yes"] [1 0 "No"] [2 0 "Yes"]] "=AGGREGATE(15,6,ROW(A1:A5)/(A1:A3=\"Yes\"),2)"))
      "arrays of different sizes: #N/A beyond the smaller, ignored")
  (is (near? 0 (run [[0 1 5]] "=INDEX(B:B,3)")) "a whole column counts past its used cells")
  (is (near? 9 (run [[0 0 "Apple pie"] [0 1 9]] "=VLOOKUP(\"App\"&\"*\",A1:B1,2,0)")) "VLOOKUP wildcards")
  (is (near? 14 (run [] "=SUM(10^{2;1;0}*SMALL({4;1;0},{1;2;3}))")) "SMALL with an array of k"))

(deftest approximate-lookups-skip-other-types
  (let [d [[0 0 5] [1 0 2] [2 0 9] [0 1 "a"] [1 1 "b"] [2 1 "c"]]]
    (is (near? 3 (run d "=MATCH(2,1/(A1:A3>4))")) "errors are skipped: the last match")
    (is (= "c" (run d "=LOOKUP(2,1/(A1:A3>4),B1:B3)")))
    (is (near? 10 (run [[0 0 1] [1 0 "x"] [2 0 5] [0 1 10] [1 1 20] [2 1 30]] "=VLOOKUP(3,A1:B3,2)"))
        "text among numbers is skipped")))

(defn- recalc-cells [cells]
  (e/recalc (reduce (fn [wb [r col v]] (e/set-cell wb (c/pack 0 r col) v)) (e/empty-workbook) cells)))

(deftest a-whole-column-sees-the-formulas-in-it
  (let [wb (recalc-cells [[0 1 5] [1 1 "=1+1"] [2 2 "=INDEX(B:B,2)"] [3 2 "=SUM(B:B)"]])]
    (is (near? 2 (:v (e/get-cell wb (c/pack 0 2 2)))) "computed in the same recalc")
    (is (near? 7 (:v (e/get-cell wb (c/pack 0 3 2)))))))

(deftest references-that-are-not-reads-make-no-cycle
  (let [wb (recalc-cells [[0 0 "=SUM(ROW($1:$3))"] [1 0 "=ROWS(A1:A3)+COLUMNS(A1:B1)"]])]
    (is (near? 6 (:v (e/get-cell wb (c/pack 0 0 0)))) "ROW of rows the formula is in")
    (is (near? 5 (:v (e/get-cell wb (c/pack 0 1 0))))))
  (let [wb (recalc-cells [[0 1 5] [1 1 "=INDEX(B:B,1)+1"] [2 1 "=MAX(B$1:B2)+INDEX(B:B,2)"]])]
    (is (near? 6 (:v (e/get-cell wb (c/pack 0 1 1)))) "INDEX over its own column reads one cell")
    (is (near? 12 (:v (e/get-cell wb (c/pack 0 2 1))))))
  (let [wb (recalc-cells [[0 0 "=B1+1"] [0 1 "=A1+1"]])]
    (is (= :ref (:v (e/get-cell wb (c/pack 0 0 0)))) "a circular reference")
    (is (= :ref (:v (e/get-cell wb (c/pack 0 0 1)))))))

(def ^:private g3 [[0 0 1] [1 0 2] [2 0 3] [0 1 10] [1 1 20] [2 1 30] [0 2 100] [1 2 200] [2 2 300]])

(deftest offset-with-arrays-is-an-array-of-references
  (is (near? 666 (run g3 "=SUMPRODUCT(SUBTOTAL(9,OFFSET(A1:A3,,{0,1,2})))")) "one total per column")
  (is (near? 333 (run g3 "=SUM(SUBTOTAL(4,OFFSET(A1:A3,,ROW($1:$3)-1,)))")))
  (is (near? 6 (run g3 "=SUMPRODUCT(N(OFFSET(A1,{0,1,2},0)))")))
  (is (near? 550 (run g3 "=SUM(SUMIF(A1:A3,\">1\",OFFSET(A1:A3,,{1,2})))")))
  (is (= :value (run g3 "=OFFSET(A1,{0,1},0)")) "not a value a cell can hold"))

(deftest a-smaller-sum-range-takes-the-size-of-the-range
  (is (near? 650 (run g3 "=SUMIF(A1:B3,\">1\",B1:B1)")) "B1:C3")
  (is (near? 25 (run g3 "=AVERAGEIF(A1:A3,\">1\",B1:B2)")) "B1:B3"))

(deftest offset-and-indirect-on-the-formulas-sheet
  (let [wb (-> (e/empty-workbook ["S1" "S2"])
               (e/set-cell (c/pack 1 0 0) 7)
               (e/set-cell (c/pack 1 1 1) "=OFFSET(A1,0,0)+INDIRECT(\"A1\")")
               e/recalc)]
    (is (near? 14 (:v (e/get-cell wb (c/pack 1 1 1)))))))

(deftest text-as-a-number
  (doseq [[f want] [["=\"1,000\"+0" 1000] ["=\"50%\"+0" 0.5] ["=\"$5\"*2" 10] ["=\" 7 \"+0" 7] ["=\"-1.5e2\"+0" -150]]]
    (is (near? want (run [] f)) f))
  (doseq [f ["=\"1d\"+0" "=\"NaN\"+0" "=\"12abc\"+0"]]
    (is (= :value (run [] f)) f)))

(deftest typographic-quotes-are-a-name
  (is (near? 1 (run [] "=IFERROR(1,“”)")))
  (is (= :name (run [] "=IFERROR(1/0,“”)"))))

(deftest external-books-as-cached
  (let [wb (-> (e/empty-workbook ["Main"])
               (e/define-external-sheet "1" "May 2021" [[0 0 {:t :str :v "k"}] [0 1 {:t :num :v 4.0}] [1 1 {:t :num :v 6.0}]])
               (e/set-cell (c/pack 0 0 0) "='[1]May 2021'!B1")
               (e/set-cell (c/pack 0 1 0) "=SUM([1]'May 2021'!B:B)")
               (e/set-cell (c/pack 0 2 0) "=VLOOKUP(\"k\",'[1]May 2021'!A:B,2,0)")
               (e/set-cell (c/pack 0 3 0) "=[2]Other!A1")
               (e/set-cell (c/pack 0 4 0) "=SHEETS()")
               e/recalc)
        v #(:v (e/get-cell wb (c/pack 0 % 0)))]
    (is (near? 4 (v 0)) "the book inside the quotes")
    (is (near? 10 (v 1)))
    (is (near? 4 (v 2)))
    (is (= :ref (v 3)) "a book the file does not cache")
    (is (near? 1 (v 4)) "not one of the workbook's sheets")))

(deftest sheet-names-ignore-case
  (let [wb (-> (e/empty-workbook ["Main" "Jan"])
               (e/set-cell (c/pack 1 0 0) 3)
               (e/set-cell (c/pack 0 0 0) "=jan!A1+INDIRECT(\"JAN!A1\")")
               (e/set-cell (c/pack 0 1 0) "=INDIRECT(\"Nope!A1\")")
               e/recalc)]
    (is (near? 6 (:v (e/get-cell wb (c/pack 0 0 0)))))
    (is (= :ref (:v (e/get-cell wb (c/pack 0 1 0)))) "a sheet the workbook does not have")))

(deftest criteria-read-text-as-excel-does
  (let [d [[0 0 "3-1-21"] [1 0 "3-2-21"] [2 0 44256] [0 1 1] [1 1 2] [2 1 4]]]
    (is (near? 5 (run d "=SUMIFS(B1:B3,A1:A3,DATE(2021,3,1))")) "date text matches the date")
    (is (near? 2 (run [[0 0 "1,000"] [1 0 1000]] "=COUNTIF(A1:A2,1000)")))))

(deftest a-reader-of-a-spill-follows-it
  (let [wb (recalc-cells [[0 5 "=E2*10"] [0 4 "=SEQUENCE(3)"] [5 5 "=SUM(E1:E3)"]])]
    (is (near? 20 (:v (e/get-cell wb (c/pack 0 0 5)))) "computed after the spill, in the same recalc")
    (is (near? 6 (:v (e/get-cell wb (c/pack 0 5 5)))))))

(deftest lookups-do-not-cross-types
  (is (= :na (run [[0 0 1] [1 0 2]] "=MATCH(\"1\",A1:A2,0)")) "text is not a number")
  (is (= :na (run [[0 0 "1"]] "=MATCH(1,A1:A2,0)")))
  (is (= :na (run [[1 0 "x"]] "=MATCH(\"\",A1:A2,0)")) "\"\" is not an empty cell")
  (is (= :na (run [[0 0 "a"] [0 1 1]] "=VLOOKUP(\"\",A1:B2,2,0)"))))

(deftest edits-reach-whole-column-readers
  (let [wb (recalc-cells [[0 0 "=SUM(B:B)"] [0 2 "=COUNTA(3:3)"] [0 3 "=SUM(A:XFD)-A1-C1"]])
        wb (e/recalc (-> wb (e/set-cell (c/pack 0 5000 1) 7) (e/set-cell (c/pack 0 2 16000) "x")))
        v #(:v (e/get-cell wb (c/pack 0 0 %)))]
    (is (near? 7 (v 0)) "a cell deep in the column")
    (is (near? 1 (v 2)) "a cell far along the row")
    (is (near? 7 (v 3)) "the whole sheet"))
  (let [wb (recalc-cells [[0 0 1] [1 0 "=A1*2"] [2 0 "=A2+1"]])
        ;; a formula overwritten by a value, then its input edited
        wb (e/recalc (e/set-cell wb (c/pack 0 1 0) 10))
        wb (e/recalc (e/set-cell wb (c/pack 0 0 0) 5))]
    (is (near? 11 (:v (e/get-cell wb (c/pack 0 2 0)))))))
