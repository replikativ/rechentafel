(ns spreadsheetbench-coverage
  "Conformance of rechentafel against real workbooks: every formula cell's
   value after `poi/load-workbook` (our recalculation) against the value Excel
   saved in the file. Built for SpreadsheetBench (912 tasks, CC BY-SA 4.0),
   whose grading needs the formula values of the answer ranges; any directory
   of .xlsx files works.

     clojure -M:poi -i dev/spreadsheetbench_coverage.clj \\
       -e \"(spreadsheetbench-coverage/run! \\\"<dir>\\\" {:limit 300 :out \\\"report.edn\\\"})\"

   A cell without a saved value (a formula written by a library that does not
   compute) is not compared. Numbers compare with a relative tolerance of
   1e-9; strings, booleans and error codes exactly."
  (:refer-clojure :exclude [run!])
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [rechentafel.cell :as cell]
            [rechentafel.eval :as e]
            [rechentafel.functions.all]
            [rechentafel.poi :as poi])
  (:import [java.io FileInputStream]
           [java.util.concurrent Executors TimeUnit Future TimeoutException]
           [org.apache.poi.ss.usermodel Cell CellType FormulaError Row Sheet WorkbookFactory]))

(defn- saved-value
  "The value Excel saved for formula cell `c`, as rechentafel's tagged shape,
   or nil when none was saved."
  [^Cell c]
  (try
    ;; no <v> element: nothing was saved (POI would report numeric 0)
    (when (or (not (instance? org.apache.poi.xssf.usermodel.XSSFCell c))
              (.isSetV (.getCTCell ^org.apache.poi.xssf.usermodel.XSSFCell c)))
    (condp = (.getCachedFormulaResultType c)
      CellType/NUMERIC {:t :num :v (.getNumericCellValue c)}
      CellType/STRING (let [s (.getStringCellValue c)] {:t :str :v s})
      CellType/BOOLEAN {:t :bool :v (.getBooleanCellValue c)}
      CellType/ERROR {:t :err :v (.getString (FormulaError/forInt (.getErrorCellValue c)))}
      nil))
    (catch Exception _ nil)))

(defn- ours [v]
  (cond
    (nil? v) {:t :blank}
    (map? v) v
    :else {:t :other :v v}))

(def ^:private excel-error
  "rechentafel's error keywords as Excel writes them."
  {:div0 "#DIV/0!" :ref "#REF!" :na "#N/A" :value "#VALUE!" :name "#NAME?" :num "#NUM!"
   :null "#NULL!" :spill "#SPILL!" :calc "#CALC!" :getting-data "#GETTING_DATA"})

(defn- same? [{st :t sv :v} {ot :t ov :v}]
  (cond
    (and (= st :num) (= ot :num))
    (let [a (double sv) b (double ov)]
      (or (== a b) (<= (Math/abs (- a b)) (* 1e-9 (max (Math/abs a) (Math/abs b) 1e-300)))))
    ;; Excel saves an empty-string result for a blank reference
    (and (= st :str) (= "" sv) (= ot :blank)) true
    (and (= st :err) (= ot :err)) (= (str/upper-case (str sv))
                                     (str/upper-case (or (excel-error ov) (str ov))))
    :else (and (= st ot) (= sv ov))))

(defn- functions-of [formula]
  (distinct (map second (re-seq #"([A-Z][A-Z0-9.]*)\(" (str/upper-case (str formula))))))

(defn- shape
  "A formula with its references and numbers normalized: a formula filled down
   a column is one shape."
  [formula]
  (-> (str formula)
      (str/replace #"\$?[A-Z]{1,3}\$?\d+" "R")
      (str/replace #"\b\d+(\.\d+)?\b" "N")))

(defn check-file
  "`{:file :cells :compared :mismatches [..] :error}` for one workbook."
  [^java.io.File f]
  (try
    (let [wb (poi/load-workbook (.getPath f))]
      (with-open [in (FileInputStream. f)
                  p (WorkbookFactory/create in)]
        (let [rows (for [si (range (.getNumberOfSheets p))
                         :let [^Sheet sh (.getSheetAt p si)]
                         ^Row r (iterator-seq (.iterator sh))
                         ^Cell c (iterator-seq (.iterator r))
                         :when (= CellType/FORMULA (.getCellType c))]
                     (let [saved (saved-value c)]
                       (when saved
                         (let [got (ours (e/get-cell wb (cell/pack si (.getRowIndex c) (.getColumnIndex c))))]
                           (when-not (same? saved got)
                             {:file (.getName f)
                              :array? (.isPartOfArrayFormulaGroup c)
                              :sheet (.getSheetName sh) :cell (.formatAsString (org.apache.poi.ss.util.CellReference. c))
                              :formula (.getCellFormula c) :saved saved :got got})))))]
          {:file (.getName f)
           :unparsed (:load-errors wb)
           :compared (count (filter some? (for [si (range (.getNumberOfSheets p))
                                                  :let [^Sheet sh (.getSheetAt p si)]
                                                  ^Row r (iterator-seq (.iterator sh))
                                                  ^Cell c (iterator-seq (.iterator r))
                                                  :when (= CellType/FORMULA (.getCellType c))]
                                              (saved-value c))))
           :mismatches (vec (remove nil? rows))})))
    (catch Throwable t
      {:file (.getName f) :error (str (.getSimpleName (class t)) ": " (ex-message t))})))

(defn- with-timeout [ms f]
  (let [ex (Executors/newSingleThreadExecutor)
        ^Future fut (.submit ex ^Callable f)]
    (try (.get fut ms TimeUnit/MILLISECONDS)
         (catch TimeoutException _ (.cancel fut true) ::timeout)
         (finally (.shutdownNow ex)))))

(defn run!
  "Check up to `:limit` .xlsx files under `dir` (sorted, every file when
   nil), each within `:timeout-ms` (default 60 s). Writes the full report to
   `:out` and returns the summary."
  [dir {:keys [limit out timeout-ms] :or {timeout-ms 60000}}]
  (let [files (cond->> (sort-by #(.getPath ^java.io.File %)
                                (filter #(str/ends-with? (.getName ^java.io.File %) ".xlsx") (file-seq (io/file dir))))
                limit (take limit))
        results (vec (map-indexed
                      (fn [i f]
                        (when (zero? (mod i 50)) (println "..." i (.getName ^java.io.File f)) (flush))
                        (let [r (with-timeout timeout-ms #(check-file f))]
                          (if (= ::timeout r) {:file (.getName ^java.io.File f) :error "timeout"} r)))
                      files))
        ;; a saved 0 where we compute another number: the file was saved
        ;; without recalculation (stale), not evidence against us
        stale? #(and (= {:t :num :v 0.0} (:saved %)) (= :num (get-in % [:got :t])))
        all-mismatches (mapcat :mismatches results)
        mismatches (remove stale? all-mismatches)
        unparsed (mapcat :unparsed results)
        summary {:files (count results)
                 :formulas-unparsed (count unparsed)
                 :unparsed-errors (->> unparsed (map #(first (str/split (str (:error %)) #" at | near "))) frequencies (sort-by (comp - val)) (take 12) vec)
                 :load-errors (count (filter :error results))
                 :error-kinds (frequencies (map #(first (str/split (:error %) #":")) (filter :error results)))
                 :cells-compared (reduce + (keep :compared results))
                 :cells-mismatched (count mismatches)
                 :cells-stale-zero (count (filter stale? all-mismatches))
                 :files-with-mismatches (count (filter #(seq (:mismatches %)) results))
                 :mismatch-by-function (->> mismatches (mapcat #(functions-of (:formula %)))
                                            frequencies (sort-by (comp - val)) (take 30) vec)
                 :mismatch-kinds (frequencies (map #(vector (get-in % [:saved :t]) (get-in % [:got :t])) mismatches))
                 :mismatches-in-array-formulas (count (filter :array? mismatches))
                 :distinct-shapes (count (distinct (map (comp shape :formula) mismatches)))}
        shapes (->> mismatches
                    (group-by (juxt (comp shape :formula) #(get-in % [:saved :t]) #(get-in % [:got :t])))
                    (map (fn [[[sh st gt] ms]] {:shape sh :saved st :got gt :cells (count ms)
                                                :files (count (distinct (map :file ms)))
                                                :example (first ms)}))
                    (sort-by (comp - :files)) vec)]
    (when out
      (spit out (with-out-str (pp/pprint {:summary summary
                                          :errors (filterv :error results)
                                          :unparsed (vec (take 200 unparsed))
                                          :shapes (vec (take 300 shapes))}))))
    summary))
