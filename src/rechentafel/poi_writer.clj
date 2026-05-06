(ns rechentafel.poi-writer
  "Optional XLSX writer. Saves a spread workbook back to a .xlsx via
  Apache POI, including:

    - cells: values and formula text (re-rendered from our AST via
      `rechentafel.unparse`)
    - sheets, with sheet names preserved
    - workbook-level defined names (`wb[:names]`)
    - Excel tables (`wb[:tables]`) → xl/tables/table*.xml with proper
      ListObject metadata
    - dynamic-array spill anchors (`wb[:spills]`) → emitted as the
      legacy `<f t=\"array\" ref=\"…\"/>` shape PLUS the modern
      `cm=\"1\"` attribute and a hand-built `xl/metadata.xml` with
      `xda:dynamicArrayProperties` so Excel 365 recognises them as
      dynamic arrays rather than CSE arrays.

  Like `rechentafel.poi`, this namespace is only on the classpath under
  the `:poi` or `:dev` deps alias — POI is not a runtime dependency
  of the library."
  (:require [rechentafel.cell :as cell]
            [rechentafel.eval :as e]
            [rechentafel.mtv :as mtv]
            [rechentafel.rc :as rc]
            [rechentafel.unparse :as unparse]
            [rechentafel.value :as val])
  (:import [java.io FileOutputStream ByteArrayOutputStream OutputStream]
           [org.apache.poi.ss.usermodel Workbook Sheet Row Cell CellType]
           [org.apache.poi.ss.util AreaReference CellReference CellRangeAddress]
           [org.apache.poi.xssf.usermodel XSSFWorkbook XSSFSheet XSSFCell
            XSSFTable XSSFRow]
           [org.apache.poi.openxml4j.opc OPCPackage PackagePart
            PackagePartName PackagingURIHelper
            TargetMode]
           [org.apache.poi.ooxml POIXMLDocumentPart]
           [org.openxmlformats.schemas.spreadsheetml.x2006.main
            CTCell CTCellFormula STCellFormulaType
            CTTable CTTableColumn CTTableColumns CTDefinedName
            CTDefinedNames]))

;; ---------------------------------------------------------------------------
;; Cell writes — value or formula

(defn- a1 [^long row ^long col]
  (str (CellReference/convertNumToColString (int col)) (inc row)))

(defn- area-ref [_sheet r0 c0 r1 c1]
  (str (a1 (long r0) (long c0)) ":" (a1 (long r1) (long c1))))

(defn- write-scalar! [^XSSFCell cell v]
  (case (:t v)
    :num   (.setCellValue cell (double (:v v)))
    :str   (.setCellValue cell ^String (:v v))
    :bool  (.setCellValue cell (boolean (:v v)))
    :err   nil    ;; POI's error handling is finicky; skip
    :blank nil
    nil))

(defn- set-raw-formula!
  "Install a formula string on a cell, bypassing POI's strict parser.
  POI's setCellFormula validates against its built-in function table
  and rejects names it doesn't know (LAMBDA, structured refs to tables
  POI hasn't indexed, dynamic-array functions). For our purposes, the
  formula source is authored by us and Excel will parse it correctly
  on open — so we write directly to the underlying CTCell.

  When `array-ref` is non-nil, the formula is marked as an array
  formula (`<f t=\"array\" ref=\"...\"/>`) covering the given range."
  [^XSSFCell cell ^String ftxt ^String array-ref]
  (let [ct      (.getCTCell cell)
        ctf     (or (.getF ct) (.addNewF ct))]
    (.setStringValue ctf ftxt)
    (when array-ref
      (.setT ctf STCellFormulaType/ARRAY)
      (.setRef ctf array-ref))))

(defn- formula-text
  "Render a stored RC-normalised AST back to source for cell row/col."
  [rc-ast ^long row ^long col]
  (-> rc-ast
      (rc/resolve-at row col)
      unparse/unparse))

(defn- ensure-row [^XSSFSheet sheet ^long row-idx]
  (or (.getRow sheet (int row-idx))
      (.createRow sheet (int row-idx))))

(defn- ensure-cell [^XSSFSheet sheet ^long row-idx ^long col-idx]
  (let [r (ensure-row sheet row-idx)]
    (or (.getCell r (int col-idx))
        (.createCell r (int col-idx)))))

(defn- column-cells
  "Yield [row-idx tagged-value] pairs for every populated cell of the
  given MTV column up to its :max-row."
  [col]
  (when col
    (let [max-r (long (:max-row col -1))]
      (when (>= max-r 0)
        (for [r (range 0 (inc max-r))
              :let [v (mtv/col-get col r)]
              :when (and v (not= :blank (:t v)))]
          [r v])))))

(defn- write-cells! [^XSSFWorkbook wb-out spread-wb]
  (let [sheets   (:sheets spread-wb)
        formulas (:formulas spread-wb)
        spills   (:spills spread-wb)
        ;; Cell-ids that are siblings of a successful spill — emitted
        ;; with cached values but without their own formula. The
        ;; anchor's array-formula implies them.
        sibling-of (reduce (fn [acc [anchor-id {:keys [r0 c0 r1 c1 error]}]]
                             (if error
                               acc
                               (let [s (cell/sheet anchor-id)]
                                 (reduce (fn [acc [r c]]
                                           (let [id (cell/pack s r c)]
                                             (if (= id anchor-id) acc
                                                 (assoc acc id anchor-id))))
                                         acc
                                         (for [r (range r0 (inc r1))
                                               c (range c0 (inc c1))]
                                           [r c])))))
                           {} spills)]
    (doseq [si (range (count sheets))]
      (let [^XSSFSheet xs (.getSheetAt wb-out si)
            mtv-sheet (get sheets si)]
        (doseq [[col-idx col] (map-indexed vector mtv-sheet)
                [row-idx v]   (column-cells col)]
          (let [id   (cell/pack si row-idx col-idx)
                xc   (ensure-cell xs row-idx col-idx)
                spill (get spills id)]
            (cond
              ;; Anchor of a successful spill → array formula + cm.
              ;; Try POI's high-level setArrayFormula first (so its
              ;; internal arrayFormulas map is populated, which makes
              ;; isPartOfArrayFormulaGroup return true on read-back);
              ;; fall back to raw CTCell mutation when POI's parser
              ;; rejects the formula (LAMBDA, structured refs, etc.).
              (and (contains? formulas id) spill (not (:error spill)))
              (let [{:keys [r0 c0 r1 c1]} spill
                    ftxt (formula-text (get formulas id) row-idx col-idx)
                    rng-str (area-ref si r0 c0 r1 c1)
                    cra  (CellRangeAddress/valueOf rng-str)]
                (try
                  (.setArrayFormula xs ftxt cra)
                  (catch Throwable _
                    (set-raw-formula! xc ftxt rng-str)))
                (let [xc' (ensure-cell xs row-idx col-idx)]
                  (write-scalar! xc' v)
                  (.setCm (.getCTCell xc') (long 1))))

              ;; Sibling slot of a successful spill — cached value only
              (contains? sibling-of id)
              (write-scalar! xc v)

              ;; Regular formula cell
              (contains? formulas id)
              (let [ftxt (formula-text (get formulas id) row-idx col-idx)]
                (write-scalar! xc v)              ;; cached value
                (set-raw-formula! xc ftxt nil))

              ;; Plain literal
              :else
              (write-scalar! xc v))))))))

;; ---------------------------------------------------------------------------
;; Defined names

(def ^:private ^java.lang.reflect.Field xssfname-ctname-field
  ;; XSSFName wraps a CTDefinedName in a private field. setRefersToFormula
  ;; goes through POI's strict parser, which rejects LAMBDA and other
  ;; modern fns. To bypass, we reach into the field directly via
  ;; reflection and call setStringValue on the CT bean.
  (doto (.getDeclaredField org.apache.poi.xssf.usermodel.XSSFName "_ctName")
    (.setAccessible true)))

(defn- write-defined-names! [^XSSFWorkbook wb-out spread-wb]
  (doseq [[nm ast] (:names spread-wb)]
    (let [n    (.createName wb-out)
          ftxt (unparse/unparse ast)]
      (.setNameName n nm)
      (let [^CTDefinedName ct (.get xssfname-ctname-field n)]
        (.setStringValue ct ftxt)))))

;; ---------------------------------------------------------------------------
;; Tables

(defn- write-tables! [^XSSFWorkbook wb-out spread-wb]
  (doseq [[_ table] (:tables spread-wb)]
    (let [{:keys [name sheet ref columns header-rows totals-rows]} table
          ^XSSFSheet xs (.getSheetAt wb-out (int sheet))
          [r0 c0 r1 c1] ref
          ar (AreaReference. (area-ref sheet r0 c0 r1 c1)
                             (.getSpreadsheetVersion wb-out))
          ^XSSFTable t (.createTable xs ar)
          ct (.getCTTable t)]
      (.setName ct name)
      (.setDisplayName ct name)
      (.setRef ct (area-ref sheet r0 c0 r1 c1))
      (.setHeaderRowCount ct (long header-rows))
      (when (pos? totals-rows)
        (.setTotalsRowCount ct (long totals-rows))
        (.setTotalsRowShown ct true))
      (let [cols (.addNewTableColumns ct)]
        (.setCount cols (long (count columns)))
        (doseq [[i ^String cname] (map-indexed vector columns)]
          (let [c (.addNewTableColumn cols)]
            (.setId c (inc (long i)))
            (.setName c cname)))))))

;; ---------------------------------------------------------------------------
;; Dynamic-array metadata (xl/metadata.xml + relationship)
;;
;; We hand-roll the XML because POI doesn't provide a high-level helper
;; class for the futureMetadata / cellMetadata blocks needed by Excel
;; 365 to recognise dynamic-array formulas. The structure is fixed
;; for our purposes (one shared XLDAPR record everyone references):
;;
;;   <metadata>
;;     <metadataTypes count="1">
;;       <metadataType name="XLDAPR" .../>
;;     </metadataTypes>
;;     <futureMetadata count="1" name="XLDAPR">
;;       <bk><extLst><ext xmlns:xda="..." uri="...">
;;         <xda:dynamicArrayProperties fDynamic="1" fCollapsed="0"/>
;;       </ext></extLst></bk>
;;     </futureMetadata>
;;     <cellMetadata count="1"><bk><rc t="1" v="0"/></bk></cellMetadata>
;;   </metadata>

(def ^:private metadata-xml
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
       "<metadata xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\""
       " xmlns:xda=\"http://schemas.microsoft.com/office/spreadsheetml/2017/dynamicarray\""
       " xmlns:xlrd=\"http://schemas.microsoft.com/office/spreadsheetml/2017/richdata\">"
       "<metadataTypes count=\"1\">"
       "<metadataType name=\"XLDAPR\" minSupportedVersion=\"120000\""
       " copy=\"1\" pasteAll=\"1\" pasteValues=\"1\" merge=\"1\""
       " splitFirst=\"1\" rowColShift=\"1\" clearFormats=\"1\""
       " clearComments=\"1\" assign=\"1\" coerce=\"1\" cellMeta=\"1\"/>"
       "</metadataTypes>"
       "<futureMetadata count=\"1\" name=\"XLDAPR\">"
       "<bk><extLst>"
       "<ext xmlns:xda=\"http://schemas.microsoft.com/office/spreadsheetml/2017/dynamicarray\""
       " uri=\"{bdbb8cdc-fa1e-496e-a857-3c3f30c029c3}\">"
       "<xda:dynamicArrayProperties fDynamic=\"1\" fCollapsed=\"0\"/>"
       "</ext></extLst></bk>"
       "</futureMetadata>"
       "<cellMetadata count=\"1\"><bk><rc t=\"1\" v=\"0\"/></bk></cellMetadata>"
       "</metadata>"))

(def ^:private metadata-content-type
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheetMetadata+xml")

(def ^:private metadata-rel-type
  "http://schemas.microsoft.com/office/2017/06/relationships/sheetMetadata")

(defn- inject-metadata-part!
  "If the workbook has any successful spill anchors, add xl/metadata.xml
  and the workbook→metadata relationship so the cm=\"1\" markers are
  meaningful to Excel 365."
  [^XSSFWorkbook wb-out spread-wb]
  (when (some (fn [[_ sp]] (not (:error sp))) (:spills spread-wb))
    (let [^OPCPackage pkg (.getPackage wb-out)
          part-name (PackagingURIHelper/createPartName "/xl/metadata.xml")
          part      (.createPart pkg part-name metadata-content-type)
          out       (.getOutputStream part)]
      (.write out (.getBytes ^String metadata-xml "UTF-8"))
      (.close out)
      ;; Relationship from the workbook part to the metadata part.
      (let [wb-part ^POIXMLDocumentPart wb-out
            wb-pkg-part (.getPackagePart wb-part)]
        (.addRelationship wb-pkg-part part-name TargetMode/INTERNAL
                          metadata-rel-type "rIdMetadata")))))

;; ---------------------------------------------------------------------------
;; Public entry

(defn save-workbook
  "Serialise `spread-wb` to `path` as an XLSX file. Cells, formulas,
  defined names, tables, and dynamic-array spill anchors are
  preserved. Pre-existing files at `path` are overwritten."
  [spread-wb ^String path]
  (with-open [wb-out (XSSFWorkbook.)]
    (doseq [[name idx] (sort-by val (:sheet-names spread-wb))]
      (.createSheet wb-out ^String name))
    (write-tables! wb-out spread-wb)
    (write-defined-names! wb-out spread-wb)
    (write-cells! wb-out spread-wb)
    (inject-metadata-part! wb-out spread-wb)
    (with-open [out (FileOutputStream. path)]
      (.write wb-out out))))
