(ns rechentafel.poi
  "Optional XLSX loader. Reads a `.xlsx` / `.xls` via Apache POI and
  replays every cell into a pure-Clojure workbook built by `rechentafel.eval`.

  POI is not a runtime dependency of the library — this namespace is
  only on the classpath under the `:poi` or `:dev` deps alias.

      (require '[rechentafel.poi :as poi] '[rechentafel.eval :as e]
               '[rechentafel.cell :as c] '[rechentafel.functions.all])
      (def wb (poi/load-workbook \"model.xlsx\"))
      (e/get-cell wb (c/pack 0 0 0))

  Only values and formula *strings* are copied. Formulas are re-parsed
  and re-evaluated by our interpreter, so cached POI results are ignored
  by design."
  (:require [rechentafel.eval :as e]
            [rechentafel.cell :as cell])
  (:import [java.io FileInputStream]
           [org.apache.poi.ss.usermodel WorkbookFactory Workbook Sheet Row Cell
            CellType Name]
           [org.apache.poi.xssf.usermodel XSSFSheet XSSFTable]
           [org.apache.poi.ss.util AreaReference CellReference]))

(defn- cell-input
  "POI Cell → the shape `set-cell` expects (formula string, number, string,
  boolean, or nil for blank)."
  [^Cell c]
  (condp = (.getCellType c)
    CellType/FORMULA (str "=" (.getCellFormula c))
    CellType/NUMERIC (.getNumericCellValue c)
    CellType/STRING  (.getStringCellValue c)
    CellType/BOOLEAN (.getBooleanCellValue c)
    CellType/BLANK   nil
    CellType/ERROR   nil
    CellType/_NONE   nil))

(defn- array-formula-sibling?
  "Cells in an array-formula range that are NOT the top-left anchor.
  POI replicates the formula across all cells of the range, but only
  the anchor should drive evaluation in our engine — the rest get
  filled in when the spill materialises. Detect the anchor by
  comparing the cell's coords to the range's first row/col."
  [^Cell c]
  (when (.isPartOfArrayFormulaGroup c)
    (let [rng (.getArrayFormulaRange c)]
      (or (not= (.getRowIndex c) (.getFirstRow rng))
          (not= (int (.getColumnIndex c)) (.getFirstColumn rng))))))

(defn- load-cells [wb ^Workbook poi]
  (let [n-sheets (.getNumberOfSheets poi)]
    (loop [si 0, wb wb]
      (if (>= si n-sheets)
        wb
        (let [sheet (.getSheetAt poi si)
              rows  (iterator-seq (.iterator sheet))
              wb'   (reduce
                     (fn [wb ^Row r]
                       (reduce
                        (fn [wb ^Cell c]
                          (cond
                            (array-formula-sibling? c) wb
                            :else
                            (let [input (cell-input c)]
                              (if (nil? input) wb
                                  (e/set-cell wb
                                              (cell/pack si
                                                         (int (.getRowIndex c))
                                                         (int (.getColumnIndex c)))
                                              input)))))
                        wb
                        (iterator-seq (.iterator r))))
                     wb
                     rows)]
          (recur (inc si) wb'))))))

(defn- sheet-names [^Workbook poi]
  (vec (for [i (range (.getNumberOfSheets poi))]
         (.getSheetName (.getSheetAt poi i)))))

(defn- table-spec
  "Convert one XSSFTable into the spec map `eval/define-table` expects.
  Returns `[name spec]` or nil if POI didn't expose a usable area."
  [^XSSFTable t sheet-idx]
  (when-let [^AreaReference area (.getArea t)]
    (let [first-c (.getFirstCell area)
          last-c  (.getLastCell area)
          r0 (.getRow first-c)
          c0 (.getCol first-c)
          r1 (.getRow last-c)
          c1 (.getCol last-c)
          ct  (.getCTTable t)
          name (.getName ct)
          ;; Column names live on CTTableColumn entries inside CTTable
          ;; — XSSFTable doesn't expose them directly.
          cols (vec (for [^Object col (.getTableColumnList (.getTableColumns ct))]
                      (.getName col)))
          hr  (if (.isSetHeaderRowCount ct) (.getHeaderRowCount ct) 1)
          tr  (if (.isSetTotalsRowCount ct) (.getTotalsRowCount ct) 0)]
      [name {:sheet sheet-idx
             :ref [r0 c0 r1 c1]
             :columns cols
             :header-rows hr
             :totals-rows tr}])))

(defn- load-tables [wb ^Workbook poi]
  (loop [si 0, wb wb]
    (if (>= si (.getNumberOfSheets poi))
      wb
      (let [sheet (.getSheetAt poi si)
            wb' (if (instance? XSSFSheet sheet)
                  (reduce (fn [wb ^XSSFTable t]
                            (if-let [[nm spec] (table-spec t si)]
                              (e/define-table wb nm spec)
                              wb))
                          wb (.getTables ^XSSFSheet sheet))
                  wb)]
        (recur (inc si) wb')))))

(defn- load-defined-names
  "Workbook-level defined names: name → formula. Sheet-scoped names are
  ignored for now — `eval/define-name` only supports a single global
  scope. Names with built-in roles (`_xlnm.Print_Area` etc.) are
  skipped because their semantics aren't formula-evaluation."
  [wb ^Workbook poi]
  (let [names (for [^Name nm (.getAllNames poi)
                    :when (and (not (.isHidden nm))
                               (not (.isFunctionName nm))
                               (not (.startsWith ^String (.getNameName nm)
                                                 "_xlnm.")))]
                [(.getNameName nm) (.getRefersToFormula nm)])]
    (reduce (fn [wb [nm formula]]
              (try (e/define-name wb nm (str "=" formula))
                   (catch Throwable _ wb)))    ;; skip names we can't parse
            wb names)))

(defn load-workbook
  "Read `path` with Apache POI and return a recalculated spread workbook.
  Formulas are re-evaluated by our interpreter, not POI. Excel tables
  (ListObjects) are read from xl/tables/table*.xml and registered via
  `eval/define-table`; workbook-level defined names are loaded via
  `eval/define-name`. Sheet-scoped names and `_xlnm.*` built-ins are
  skipped."
  [path]
  (with-open [in (FileInputStream. ^String path)
              poi (WorkbookFactory/create in)]
    (-> (e/empty-workbook (sheet-names poi))
        (load-tables poi)
        (load-defined-names poi)
        (load-cells poi)
        (e/recalc))))
