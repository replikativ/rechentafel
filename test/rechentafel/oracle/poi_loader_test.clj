(ns rechentafel.oracle.poi-loader-test
  "Round-trips through Apache POI's writer:
    - build a workbook with cells, named ranges, and a Table (ListObject)
    - save to a temp .xlsx
    - load via rechentafel.poi/load-workbook
    - verify cells, formulas evaluate, and `wb[:tables]` is populated"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [rechentafel.poi :as poi]
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all])
  (:import [org.apache.poi.xssf.usermodel XSSFWorkbook XSSFSheet XSSFTable]
           [org.apache.poi.ss.util AreaReference CellReference]
           [org.openxmlformats.schemas.spreadsheetml.x2006.main
            CTTable CTTableColumn CTTableColumns CTTableStyleInfo]
           [java.io FileOutputStream]))

(defn- build-fixture-with-table!
  "Write a small XLSX with a Sales table at A1:B4. Returns the path."
  [path]
  (let [wb    (XSSFWorkbook.)
        sheet (.createSheet wb "Sheet1")]
    ;; A1=Date, B1=Amount, A2..A4=d1..d3, B2..B4=10/20/30
    (let [r0 (.createRow sheet 0)]
      (.setCellValue (.createCell r0 0) "Date")
      (.setCellValue (.createCell r0 1) "Amount"))
    (doseq [[ri name amt] [[1 "d1" 10.0] [2 "d2" 20.0] [3 "d3" 30.0]]]
      (let [r (.createRow sheet ri)]
        (.setCellValue (.createCell r 0) ^String name)
        (.setCellValue (.createCell r 1) (double amt))))
    ;; Create the table
    (let [^XSSFTable table (.createTable sheet (AreaReference. "A1:B4"
                                                               (.getSpreadsheetVersion wb)))
          ct      (.getCTTable table)]
      (.setName ct "Sales")
      (.setDisplayName ct "Sales")
      (.setRef ct "A1:B4")
      (.setHeaderRowCount ct 1)
      ;; Set columns
      (let [cols (.addNewTableColumns ct)]
        (.setCount cols 2)
        (let [c1 (.addNewTableColumn cols)
              c2 (.addNewTableColumn cols)]
          (.setId c1 1) (.setName c1 "Date")
          (.setId c2 2) (.setName c2 "Amount"))))
    (with-open [out (FileOutputStream. ^String path)]
      (.write wb out))
    (.close wb)
    path))

(defn- temp-path [^String suffix]
  (let [f (java.io.File/createTempFile "spread-poi-" suffix)]
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest poi-loader-reads-tables
  (let [path (temp-path ".xlsx")
        _    (build-fixture-with-table! path)
        wb   (poi/load-workbook path)]
    (testing "tables map is populated from xl/tables/*.xml"
      (is (= 1 (count (:tables wb))))
      (is (contains? (:tables wb) "SALES")))
    (testing "structured ref resolves against the loaded table"
      (let [wb (-> wb
                   (e/set-cell (c/pack 0 6 0) "=SUM(Sales[Amount])")
                   (e/recalc))]
        (is (= 60.0 (:v (e/get-cell wb (c/pack 0 6 0)))))))
    (testing "headers area resolves to the literal 'Amount' header"
      (let [wb (-> wb
                   (e/set-cell (c/pack 0 6 1) "=Sales[[#Headers],[Amount]]")
                   (e/recalc))]
        (is (= "Amount" (:v (e/get-cell wb (c/pack 0 6 1)))))))))

(defn- build-fixture-with-defined-name! [path]
  (let [wb    (XSSFWorkbook.)
        sheet (.createSheet wb "Sheet1")]
    (.setCellValue (.createCell (.createRow sheet 0) 0) 10.0)
    (.setCellValue (.createCell (.createRow sheet 1) 0) 20.0)
    (.setCellValue (.createCell (.createRow sheet 2) 0) 30.0)
    (let [nm (.createName wb)]
      (.setNameName nm "MyTotal")
      (.setRefersToFormula nm "Sheet1!$A$1+Sheet1!$A$2+Sheet1!$A$3"))
    (with-open [out (FileOutputStream. ^String path)]
      (.write wb out))
    (.close wb)
    path))

(deftest poi-loader-reads-defined-names
  (let [path (temp-path ".xlsx")
        _    (build-fixture-with-defined-name! path)
        wb   (poi/load-workbook path)]
    (testing "defined name registered on the workbook"
      (is (contains? (:names wb) "MYTOTAL")))
    (testing "defined name evaluates inside a formula"
      (let [wb (-> wb
                   (e/set-cell (c/pack 0 5 0) "=MyTotal")
                   (e/recalc))]
        (is (= 60.0 (:v (e/get-cell wb (c/pack 0 5 0)))))))))
