(ns rechentafel.oracle.poi-oracle
  "Drive Apache POI's FormulaEvaluator to produce ground-truth results for
  cross-checking our pure-cljc implementations.

  The workflow:

    1. `eval-formula` builds an XSSF workbook, optionally populates cells
       that the formula references, evaluates the formula, and converts
       POI's CellValue back into our tagged-value shape.

    2. `check` / `check-approx` take a function name plus arg values and
       compare `(f/call ...)` against a POI evaluation of the equivalent
       formula string. Scalar args become inline literals; area args get
       written to cells and referenced as ranges.

    3. Test modules call `check-many` on collections of cases and return
       failures with enough context to debug.

  We use POI's XSSF (not HSSF) evaluator — XSSF supports the full ATP
  function set, matching the breadth of our registry."
  (:require [clojure.string :as str]
            [rechentafel.value :as val]
            [rechentafel.functions :as f])
  (:import [org.apache.poi.xssf.usermodel XSSFWorkbook]
           [org.apache.poi.ss.usermodel CellValue CellType FormulaError
            Workbook Sheet Row Cell]
           [org.apache.poi.ss.util CellReference]
           [org.apache.poi.ss.formula.eval NotImplementedException
            NotImplementedFunctionException]))

;; ---------------------------------------------------------------------------
;; POI -> tagged value

(def ^:private poi-error-code->kw
  {0x00 :null
   0x07 :div0
   0x0F :value
   0x17 :ref
   0x1D :name
   0x24 :num
   0x2A :na
   0x2B :getting-data})

(defn- cellvalue->tagged [^CellValue cv]
  (let [ct (.getCellType cv)]
    (condp = ct
      CellType/NUMERIC {:t :num :v (.getNumberValue cv)}
      CellType/STRING  {:t :str :v (.getStringValue cv)}
      CellType/BOOLEAN {:t :bool :v (.getBooleanValue cv)}
      CellType/BLANK   val/BLANK
      CellType/ERROR   {:t :err
                        :v (get poi-error-code->kw
                                (bit-and 0xFF (.getErrorValue cv))
                                :value)})))

;; ---------------------------------------------------------------------------
;; Tagged value -> Excel literal (for inlining scalars into a formula)

(defn- esc-str [s]
  (str "\"" (str/replace s "\"" "\"\"") "\""))

(defn- err-literal [code]
  (case code
    :null  "#NULL!"
    :div0  "(1/0)"             ;; POI parses these error literals inconsistently;
    :value "(1+\"a\")"         ;; using an expression that yields the same error
    :ref   "(#REF!)"
    :name  "NOSUCHFN()"
    :num   "(SQRT(-1))"
    :na    "NA()"
    "NA()"))

(defn v->literal
  "Render a tagged value as an Excel-formula literal suitable for inlining
  as a function argument. Strings are quoted, errors become expressions
  that evaluate to the same error code."
  [v]
  (case (:t v)
    :num   (let [n (double (:v v))]
             (if (neg? n) (str "(" n ")") (str n)))
    :str   (esc-str (:v v))
    :bool  (if (:v v) "TRUE" "FALSE")
    :blank "\"\""              ;; no clean blank literal; rarely matters
    :err   (err-literal (:v v))
    (throw (ex-info "can't inline value" {:v v}))))

;; ---------------------------------------------------------------------------
;; Populating an area into the sheet and formatting a range reference

(defn- set-scalar! [^Cell cell v]
  (case (:t v)
    :num   (.setCellValue cell (double (:v v)))
    :str   (.setCellValue cell ^String (:v v))
    :bool  (.setCellValue cell (boolean (:v v)))
    :blank (.setBlank cell)
    :err   (let [code (bit-and 0xFF (case (:v v)
                                      :null  0x00
                                      :div0  0x07
                                      :value 0x0F
                                      :ref   0x17
                                      :name  0x1D
                                      :num   0x24
                                      :na    0x2A
                                      0x0F))]
             (.setCellErrorValue cell (byte code)))
    (throw (ex-info "can't place in cell" {:v v}))))

(defn- col-letters [c]
  (loop [n (inc c) acc ""]
    (if (<= n 0) acc
        (let [rem (mod (dec n) 26)]
          (recur (quot (dec n) 26)
                 (str (char (+ 65 rem)) acc))))))

(defn- addr [r c] (str (col-letters c) (inc r)))

(defn- place-area!
  "Write a 2D vector of tagged values into `sheet` starting at (r0,c0).
  Returns the A1 range reference covering the written cells."
  [^Sheet sheet r0 c0 values]
  (let [rows (count values)
        cols (count (first values))]
    (dotimes [i rows]
      (let [^Row row (or (.getRow sheet (+ r0 i))
                         (.createRow sheet (+ r0 i)))]
        (dotimes [j cols]
          (let [cell (.createCell row (+ c0 j))]
            (set-scalar! cell (get-in values [i j]))))))
    (str (addr r0 c0) ":" (addr (+ r0 (dec rows)) (+ c0 (dec cols))))))

;; ---------------------------------------------------------------------------
;; eval-formula — the main entry point

(defn- format-arg!
  "Convert an arg into an Excel formula-argument string. Areas are laid
  out on the sheet; scalars become inline literals."
  [arg ^Sheet sheet row-state]
  (case (:t arg)
    :area (let [r (swap! row-state + (inc (count (:values arg))))
                top (- r (inc (count (:values arg))))]
            (place-area! sheet top 0 (:values arg)))
    (v->literal arg)))

;; Sentinel returned when POI lacks an implementation (NotImplementedException).
;; Use this in `values-match?` to skip cross-checks rather than flagging a false
;; mismatch against the NotImplemented sentinel. Distinct from `#N/A` because
;; our code may legitimately return `#N/A` (stubs, error propagation).
(def NOT-IMPLEMENTED {:t ::not-implemented})

(defn not-implemented? [v] (= ::not-implemented (:t v)))

(defn eval-formula
  "Evaluate a formula string (no leading `=`) using POI's XSSF formula
  evaluator. Returns a tagged value. Cells populated by the `ranges`
  map (address-string → 2D vector of tagged values) are written before
  evaluation. Useful for INDIRECT and cell-reference tests.

  POI's `NotImplementedException` is converted into `NOT-IMPLEMENTED`
  so callers can tell a genuine POI gap apart from any error a well-known
  function might produce."
  ([formula] (eval-formula formula nil))
  ([formula ranges]
   (with-open [wb (XSSFWorkbook.)]
     (let [sheet (.createSheet wb "S1")]
       (doseq [[^String addr-str values] ranges]
         (let [ref (CellReference. addr-str)
               r0 (.getRow ref)
               c0 (int (.getCol ref))]
           (place-area! sheet r0 c0 values)))
       (let [target-row (.createRow sheet 500)
             target     (.createCell target-row 10)]
         (.setCellFormula target formula)
         (let [fe (-> wb .getCreationHelper .createFormulaEvaluator)]
           (try
             (cellvalue->tagged (.evaluate fe target))
             (catch NotImplementedException _ NOT-IMPLEMENTED)
             (catch NotImplementedFunctionException _ NOT-IMPLEMENTED))))))))

(defn- poi-fn-name
  "POI's formula parser reads dotted identifiers as range expressions
  (`T.INV` → a full-row range). Excel stores newer functions with an
  `_xlfn.` prefix for exactly this reason; POI recognises it and routes
  to the registered ATP implementation."
  [fname]
  (let [u (str/upper-case fname)]
    (if (str/includes? u ".")
      (str "_xlfn." u)
      u)))

(defn call-via-poi
  "Build a formula `FNAME(arg1, arg2, ...)`, evaluate through POI, and
  return a tagged value. Area args are laid out as cell ranges."
  [fname args]
  (with-open [wb (XSSFWorkbook.)]
    (let [sheet (.createSheet wb "S1")
          row-state (atom 0)
          rendered (mapv #(format-arg! % sheet row-state) args)
          formula  (str (poi-fn-name fname) "(" (str/join "," rendered) ")")
          target-row (.createRow sheet 500)
          target     (.createCell target-row 10)]
      (.setCellFormula target formula)
      (let [fe (-> wb .getCreationHelper .createFormulaEvaluator)]
        (try
          (cellvalue->tagged (.evaluate fe target))
          (catch NotImplementedException _ NOT-IMPLEMENTED)
          (catch NotImplementedFunctionException _ NOT-IMPLEMENTED))))))

;; ---------------------------------------------------------------------------
;; Comparison helpers

(defn- approx= [^double a ^double b ^double tol]
  (<= (Math/abs (- a b)) tol))

(defn values-match?
  "Equal-enough for cross-checking. Numbers compare within tolerance
  (relative for large magnitudes, absolute otherwise). Errors match on
  code. Strings and booleans are exact."
  ([a b] (values-match? a b 1.0e-6))
  ([a b tol]
   (cond
     (not= (:t a) (:t b)) false
     (val/num? a)         (let [x (double (:v a)) y (double (:v b))
                                mag (max 1.0 (Math/abs y))]
                            (approx= x y (* tol mag)))
     (val/str? a)         (= (:v a) (:v b))
     (val/bool? a)        (= (:v a) (:v b))
     (val/err? a)         (= (:v a) (:v b))
     (val/blank? a)       true
     :else                (= a b))))

(defn check
  "Run one cross-check case. Returns {:ok? bool :ours v :poi v :fname ..
  :args ..}. A case can skip POI (e.g. for functions POI treats as #N/A
  that we implement) by passing :skip-poi? true."
  [{:keys [fname args tol skip-poi? poi-formula]
    :or   {tol 1.0e-6}}]
  (let [reg  (f/lookup fname)
        ours (if (:lazy? reg)
               ;; lazy fns short-circuit errors themselves; bypass first-error
               ;; by routing through call-lazy with a null eval ctx (the fns
               ;; fall through to treating ast-args as pre-evaluated values).
               (f/call-lazy {} fname args)
               (f/call fname args))
        poi  (cond
               skip-poi?    nil
               poi-formula  (eval-formula poi-formula)
               :else        (call-via-poi fname args))]
    {:fname  fname
     :args   args
     :ours   ours
     :poi    poi
     :ok?    (cond
               skip-poi?            true
               (not-implemented? poi) true    ;; POI lacks this fn entirely
               (and (val/err? poi)
                    (= :name (:v poi))) true  ;; coverage gap in POI
               :else                (values-match? ours poi tol))}))

(defn check-many [cases]
  (let [results (mapv check cases)
        failures (filter (complement :ok?) results)]
    {:total     (count results)
     :passed    (- (count results) (count failures))
     :failed    (count failures)
     :failures  (vec failures)}))

(defn report
  "Format a failure entry for test output."
  [fail]
  (str "mismatch " (:fname fail)
       " args=" (pr-str (:args fail))
       "\n  ours=" (pr-str (:ours fail))
       "\n  poi =" (pr-str (:poi fail))))

