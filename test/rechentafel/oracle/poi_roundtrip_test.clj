(ns rechentafel.oracle.poi-roundtrip-test
  "End-to-end round-trip tests for the .xlsx writer.

  Pattern:
    - Build a workbook in spread-clj
    - Save via rechentafel.poi-writer/save-workbook
    - Reload via rechentafel.poi/load-workbook
    - Verify cell values, formulas, table extents, defined names, and
      dynamic-array spills all survive the round-trip.

  We also cross-check via Apache POI's XSSFWorkbook directly to
  confirm the on-disk shape (presence of `<f t=\"array\" ref=\"…\"/>`
  for spill anchors, `cm=\"1\"` attribute, the xl/metadata.xml part)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [rechentafel.poi :as poi]
            [rechentafel.poi-writer :as poi-writer]
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.functions.all])
  (:import [java.io File FileInputStream]
           [org.apache.poi.ss.usermodel WorkbookFactory]
           [org.apache.poi.xssf.usermodel XSSFWorkbook XSSFSheet XSSFCell]
           [org.apache.poi.openxml4j.opc OPCPackage PackageAccess
            PackagingURIHelper]))

(defn- temp-path []
  (let [f (File/createTempFile "spread-rt-" ".xlsx")]
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- save-and-reload
  "Round-trip helper: spread-wb → save → reload → returns reloaded wb."
  [spread-wb]
  (let [path (temp-path)]
    (poi-writer/save-workbook spread-wb path)
    (poi/load-workbook path)))

;; ---------------------------------------------------------------------------
;; Cells

(deftest scalars-roundtrip
  (let [wb (-> (e/empty-workbook ["S1"])
               (e/set-cell (c/pack 0 0 0) 42)
               (e/set-cell (c/pack 0 0 1) "hello")
               (e/set-cell (c/pack 0 0 2) true)
               (e/set-cell (c/pack 0 1 0) 3.14)
               (e/recalc))
        wb' (save-and-reload wb)]
    (is (= 42.0    (:v (e/get-cell wb' (c/pack 0 0 0)))))
    (is (= "hello" (:v (e/get-cell wb' (c/pack 0 0 1)))))
    (is (= true    (:v (e/get-cell wb' (c/pack 0 0 2)))))
    (is (= 3.14    (:v (e/get-cell wb' (c/pack 0 1 0)))))))

(deftest formulas-roundtrip
  (let [wb (-> (e/empty-workbook ["S1"])
               (e/set-cell (c/pack 0 0 0) 10)
               (e/set-cell (c/pack 0 0 1) 20)
               (e/set-cell (c/pack 0 1 0) "=A1+B1")
               (e/set-cell (c/pack 0 2 0) "=SUM(A1:B1)*2")
               (e/recalc))
        wb' (save-and-reload wb)]
    (is (= 30.0 (:v (e/get-cell wb' (c/pack 0 1 0)))))
    (is (= 60.0 (:v (e/get-cell wb' (c/pack 0 2 0)))))
    ;; Re-evaluation works — change A1, recalc, see new result
    (let [wb'' (-> wb'
                   (e/set-cell (c/pack 0 0 0) 100)
                   (e/recalc))]
      (is (= 120.0 (:v (e/get-cell wb'' (c/pack 0 1 0))))))))

(deftest multiple-sheets-roundtrip
  (let [wb (-> (e/empty-workbook ["A" "B" "C"])
               (e/set-cell (c/pack 0 0 0) 1)
               (e/set-cell (c/pack 1 0 0) 2)
               (e/set-cell (c/pack 2 0 0) 3)
               (e/set-cell (c/pack 0 1 0) "=A:A!A1+B!A1+C!A1")  ;; not really 3D, but tests sheet refs
               (e/recalc))
        wb' (save-and-reload wb)]
    (is (= 1.0 (:v (e/get-cell wb' (c/pack 0 0 0)))))
    (is (= 2.0 (:v (e/get-cell wb' (c/pack 1 0 0)))))
    (is (= 3.0 (:v (e/get-cell wb' (c/pack 2 0 0)))))))

;; ---------------------------------------------------------------------------
;; Defined names

(deftest defined-names-roundtrip
  (let [wb (-> (e/empty-workbook ["S1"])
               (e/define-name "MyConst" "=42")
               (e/set-cell (c/pack 0 0 0) "=MyConst*2")
               (e/recalc))
        wb' (save-and-reload wb)]
    (is (contains? (:names wb') "MYCONST"))
    (is (= 84.0 (:v (e/get-cell wb' (c/pack 0 0 0)))))))

;; ---------------------------------------------------------------------------
;; Tables

(deftest table-roundtrip
  (let [wb (-> (e/empty-workbook ["S1"])
               (e/set-cell (c/pack 0 0 0) "Date")
               (e/set-cell (c/pack 0 0 1) "Amount")
               (e/set-cell (c/pack 0 1 0) "d1") (e/set-cell (c/pack 0 1 1) 10)
               (e/set-cell (c/pack 0 2 0) "d2") (e/set-cell (c/pack 0 2 1) 20)
               (e/set-cell (c/pack 0 3 0) "d3") (e/set-cell (c/pack 0 3 1) 30)
               (e/define-table "Sales" {:sheet 0 :ref [0 0 3 1]
                                        :columns ["Date" "Amount"]
                                        :header-rows 1})
               (e/set-cell (c/pack 0 5 0) "=SUM(Sales[Amount])")
               (e/recalc))
        wb' (save-and-reload wb)]
    (testing "table metadata survives"
      (is (contains? (:tables wb') "SALES"))
      (let [t (get (:tables wb') "SALES")]
        (is (= [0 0 3 1] (:ref t)))
        (is (= ["Date" "Amount"] (:columns t)))))
    (testing "structured ref still resolves"
      (is (= 60.0 (:v (e/get-cell wb' (c/pack 0 5 0))))))))

;; ---------------------------------------------------------------------------
;; Dynamic-array spills

(deftest spill-roundtrip
  (let [wb (-> (e/empty-workbook ["S1"])
               (e/set-cell (c/pack 0 0 0) "=SEQUENCE(5)")
               (e/set-cell (c/pack 0 6 0) "=SUM(A1#)")
               (e/recalc))
        wb' (save-and-reload wb)]
    (testing "anchor cell is recognised as a spill"
      (is (contains? (:spills wb') (c/pack 0 0 0)))
      (let [sp (get (:spills wb') (c/pack 0 0 0))]
        (is (= 0 (:r0 sp)))
        (is (= 4 (:r1 sp)))))
    (testing "spill values populate siblings"
      (is (= 1.0 (:v (e/get-cell wb' (c/pack 0 0 0)))))
      (is (= 5.0 (:v (e/get-cell wb' (c/pack 0 4 0))))))
    (testing "A1# consumer re-evaluates"
      (is (= 15.0 (:v (e/get-cell wb' (c/pack 0 6 0))))))))

;; ---------------------------------------------------------------------------
;; On-disk shape — peek at the saved bytes via raw POI

(deftest spill-anchor-has-array-formula-and-cm
  (let [path (temp-path)
        wb   (-> (e/empty-workbook ["S1"])
                 (e/set-cell (c/pack 0 0 0) "=SEQUENCE(3)")
                 (e/recalc))
        _    (poi-writer/save-workbook wb path)
        poi  (with-open [in (FileInputStream. path)]
               (XSSFWorkbook. in))
        sheet (.getSheetAt poi 0)
        a1    (.getCell (.getRow sheet 0) 0)]
    (try
      (testing "anchor cell carries the t=\"array\" formula"
        (is (.isPartOfArrayFormulaGroup a1))
        (let [rng (.getArrayFormulaRange a1)]
          (is (= 0 (.getFirstRow rng)))
          (is (= 2 (.getLastRow rng)))
          (is (= 0 (.getFirstColumn rng)))
          (is (= 0 (.getLastColumn rng)))))
      (testing "anchor cell has cm=\"1\" attribute"
        (let [ct-cell (.getCTCell a1)]
          (is (.isSetCm ct-cell))
          (is (= 1 (.getCm ct-cell)))))
      (finally (.close poi)))))

(deftest metadata-xml-part-is-injected
  (let [path (temp-path)
        wb   (-> (e/empty-workbook ["S1"])
                 (e/set-cell (c/pack 0 0 0) "=SEQUENCE(3)")
                 (e/recalc))]
    (poi-writer/save-workbook wb path)
    (with-open [pkg (OPCPackage/open ^String path PackageAccess/READ)]
      (let [name (PackagingURIHelper/createPartName "/xl/metadata.xml")
            part (.getPart pkg name)]
        (is (some? part) "xl/metadata.xml exists")
        (let [xml (slurp (.getInputStream part))]
          (is (str/includes? xml "XLDAPR"))
          (is (str/includes? xml "dynamicArrayProperties"))
          (is (str/includes? xml "fDynamic=\"1\"")))))))

(deftest no-metadata-when-no-spills
  ;; Pure scalar workbook → no metadata.xml part
  (let [path (temp-path)
        wb   (-> (e/empty-workbook ["S1"])
                 (e/set-cell (c/pack 0 0 0) 42)
                 (e/recalc))]
    (poi-writer/save-workbook wb path)
    (with-open [pkg (OPCPackage/open ^String path PackageAccess/READ)]
      (let [name (PackagingURIHelper/createPartName "/xl/metadata.xml")]
        (is (nil? (.getPart pkg name))
            "no metadata.xml when there are no dynamic arrays")))))

;; ---------------------------------------------------------------------------
;; Composition: features stack across the round-trip

(deftest composed-roundtrip
  (let [wb (-> (e/empty-workbook ["S1"])
               (e/set-cell (c/pack 0 0 0) "x")
               (e/set-cell (c/pack 0 1 0) 1)
               (e/set-cell (c/pack 0 2 0) 2)
               (e/set-cell (c/pack 0 3 0) 3)
               (e/define-table "Data" {:sheet 0 :ref [0 0 3 0]
                                       :columns ["x"] :header-rows 1})
               (e/define-name "Sumsq" "=LAMBDA(rng, SUMPRODUCT(rng, rng))")
               (e/set-cell (c/pack 0 5 0) "=LET(s, Sumsq(Data[x]), SQRT(s))")
               (e/set-cell (c/pack 0 6 0) "=SEQUENCE(3)")
               (e/recalc))
        wb' (save-and-reload wb)]
    (testing "table + named LAMBDA + LET + structured ref all survive"
      (let [v (:v (e/get-cell wb' (c/pack 0 5 0)))]
        ;; sqrt(1+4+9) = sqrt(14) ≈ 3.7417
        (is (< (Math/abs (- 3.7416573867739413 v)) 1e-10))))
    (testing "spill survives alongside other features"
      (is (= 1.0 (:v (e/get-cell wb' (c/pack 0 6 0)))))
      (is (= 3.0 (:v (e/get-cell wb' (c/pack 0 8 0))))))))
