(ns rechentafel.fn.info
  "Information functions (POI category: information — 24 fns).

  Most are pure type-predicates (ISNUMBER / ISTEXT / ISBLANK / ...).
  A handful care about the *original* (un-dereferenced) value — ISREF
  needs to see the :ref tag before it's resolved, ISERROR/ISERR check
  the error class. Values flow in already unwrapped by `call` except
  for errors, which `call` deliberately doesn't short-circuit for
  these predicates (lazy-ish). Since we're a strict dispatcher, we
  register these as STRICT but each fn handles errors in its arg
  list manually — we can't rely on the default error pass-through,
  because predicates like ISERROR need to see errors and return TRUE."
  (:require [clojure.string :as str]
            [rechentafel.address :as addr]
            [rechentafel.cell :as cell]
            [rechentafel.rc :as rc]
            [rechentafel.unparse :as unparse]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Error-aware predicates
;;
;; These need to see errors in their argument (ISERROR on an error
;; returns TRUE). Because `call` short-circuits on the first error
;; before the impl runs, we work around that by declaring the impl
;; via a wrapper that inspects args — but `call` already intercepted.
;; The workaround: register a fn that ALWAYS returns a value, and
;; wrap in a way that call doesn't pre-filter. Simplest: because
;; `first-error` only runs on strict calls, we either make these
;; lazy (no error pre-filter) or we bypass using a sentinel.
;;
;; We use lazy registration with a trivial `:eval` stub, since these
;; want to see the raw evaluated value. In practice the evaluator will
;; already have evaluated the child AST and produced a value — lazy fns
;; receive `ast-args` which the evaluator should pass through `(:eval ctx)`
;; for a single step. Same fallback as logical/*.

(defn- eval1 [ctx ast]
  (if-let [ev (:eval ctx)]
    (ev ctx ast)
    ast))

(defn- pred
  "Install a 1-arg lazy predicate — `:lazy?` lets us see errors as values.
  Over an array (a range, an array expression) it answers per element, as
  Excel does (ISNUMBER(SEARCH(x, A1:A9)) is an array); with `whole?` the
  value is tested as it is (ISREF: a range is a reference)."
  ([name p] (pred name p false))
  ([name p whole?]
   (let [test (fn [v] (try (val/boolean-v (p v))
                           (catch #?(:clj Exception :cljs :default) e
                             (if-let [code (:excel-error (ex-data e))] (val/error code) (throw e)))))]
     (f/register! name
                  (fn [ctx ast-args]
                    (let [v (eval1 ctx (first ast-args))]
                      (if (and (not whole?) (f/area? v))
                        (-> (dissoc v :sheet :ref-r0 :ref-r1 :ref-c0 :ref-c1)
                            (update :values (fn [rows] (mapv #(mapv test %) rows))))
                        (val/boolean-v (p v)))))
                  :arity [1 1] :lazy? true))))

(pred "ISBLANK"   val/blank?)
(pred "ISNUMBER"  val/num?)
(pred "ISTEXT"    val/str?)
(pred "ISNONTEXT" (complement val/str?))
(pred "ISLOGICAL" val/bool?)
(pred "ISREF"     #(or (val/ref? %) (and (f/area? %) (some? (:sheet %))) (some? (:ref %))) true)
(pred "ISERROR"   val/err?)
(pred "ISERR"     (fn [v] (and (val/err? v) (not= :na (:v v)))))
(pred "ISNA"      (fn [v] (and (val/err? v) (= :na (:v v)))))

(pred "ISEVEN"
      (fn [v]
        (let [n (val/to-num v)]
          (when-not (val/num? n) (f/domain-error! :value))
          (zero? (mod (long (:v n)) 2)))))

(pred "ISODD"
      (fn [v]
        (let [n (val/to-num v)]
          (when-not (val/num? n) (f/domain-error! :value))
          (not (zero? (mod (long (:v n)) 2))))))

;; ---------------------------------------------------------------------------
;; Constants / classifiers

(f/register! "NA" (fn [_args] val/ERR-NA) :arity [0 0])

(f/register! "ERROR.TYPE"
  ;; Excel error-type codes:
  ;;   #NULL! = 1, #DIV/0! = 2, #VALUE! = 3, #REF! = 4, #NAME? = 5,
  ;;   #NUM! = 6, #N/A = 7, #GETTING_DATA = 8
  ;; Needs lazy-registered: the strict dispatcher would pass the error
  ;; straight through and hide it from us.
             (fn [ctx ast-args]
               (let [v (eval1 ctx (first ast-args))]
                 (if-not (val/err? v) val/ERR-NA
                         (val/number
                          (case (:v v)
                            :null 1 :div0 2 :value 3 :ref 4 :name 5 :num 6 :na 7
                            :getting-data 8
                            (f/domain-error! :na))))))
             :arity [1 1] :lazy? true)

(f/register! "TYPE"
  ;; Excel TYPE: Number=1, Text=2, Logical=4, Error=16, Array=64.
  ;; Also lazy — needs to see errors as values.
             (fn [ctx ast-args]
               (let [v (eval1 ctx (first ast-args))]
                 (val/number
                  (case (:t v)
                    :num   1.0
                    :str   2.0
                    :bool  4.0
                    :err   16.0
                    :area  64.0
                    :blank 1.0
                    1.0))))
             :arity [1 1] :lazy? true)

;; N is registered in text.cljc (it's classed as text in POI — we keep it
;; there so both modules stay coherent with POI's categorisation).

;; ---------------------------------------------------------------------------
;; Geometry — ROW / ROWS / COLUMN / COLUMNS / AREAS

(defn- dims [v]
  (cond
    (val/area? v) [(inc (- (long (:ref-r1 v (:r1 v)))
                           (long (:ref-r0 v (:r0 v 0)))))
                   (inc (- (long (:ref-c1 v (:c1 v)))
                           (long (:ref-c0 v (:c0 v 0)))))]
    (val/ref? v)  [1 1]
    :else         [1 1]))

(f/register! "ROWS"
             (fn [args] (val/number (double (first (dims (first args))))))
             :arity [1 1])

(f/register! "COLUMNS"
             (fn [args] (val/number (double (second (dims (first args))))))
             :arity [1 1])

(defn- ref-bounds
  "`[r0 c0 r1 c1]` (0-based) of what a ROW/COLUMN argument refers to: a
   :ref or :range node read without evaluating the cells, or any other
   expression (e.g. INDIRECT) evaluated to the area or reference it yields."
  [ctx a]
  (case (:op a)
    :ref [(:row a) (:col a) (:row a) (:col a)]
    :range (let [l (:left a) r (:right a)]
             [(min (:row l) (:row r)) (min (:col l) (:col r))
              (max (:row l) (:row r)) (max (:col l) (:col r))])
    (when-let [ev (:eval ctx)]
      (let [v (ev ctx a)]
        (cond
          (val/area? v) [(:ref-r0 v (:r0 v)) (:ref-c0 v (:c0 v))
                         (:ref-r1 v (:r1 v)) (:ref-c1 v (:c1 v))]
          (val/ref? v) [(:row v) (:col v) (:row v) (:col v)])))))

(defn- numbers-area
  "A rows x cols area of (f r c) numbers; a 1x1 one is its number."
  [rows cols f]
  (if (= 1 rows cols)
    (val/number (double (f 0 0)))
    {:t :area :r0 0 :c0 0 :r1 (dec rows) :c1 (dec cols)
     :values (vec (for [r (range rows)] (vec (for [c (range cols)] (val/number (double (f r c)))))))}))

(f/register! "ROW"
  ;; Lazy: inspect the AST arg so we can read the row of a :ref/:range
  ;; node without evaluating the cell (whose value would strip ref-ness).
  ;; With no arg and an evaluator ctx that exposes :current-cell, use that.
  ;; A multi-row range gives the column of its row numbers, as in Excel
  ;; (ROW(A1:A3) = {1;2;3}).
             (fn [ctx ast-args]
               (if (empty? ast-args)
                 (if-let [cur (:current-cell ctx)]
                   (val/number (double (inc (long (:row cur)))))
                   val/ERR-VALUE)
                 (if-let [[r0 _ r1 _] (ref-bounds ctx (first ast-args))]
                   (numbers-area (inc (- (long r1) (long r0))) 1 (fn [r _] (+ (long r0) r 1)))
                   val/ERR-VALUE)))
             :arity [0 1] :lazy? true)

(f/register! "COLUMN"
  ;; A multi-column range gives the row of its column numbers
  ;; (COLUMN(A1:C1) = {1,2,3}).
             (fn [ctx ast-args]
               (if (empty? ast-args)
                 (if-let [cur (:current-cell ctx)]
                   (val/number (double (inc (long (:col cur)))))
                   val/ERR-VALUE)
                 (if-let [[_ c0 _ c1] (ref-bounds ctx (first ast-args))]
                   (numbers-area 1 (inc (- (long c1) (long c0))) (fn [_ c] (+ (long c0) c 1)))
                   val/ERR-VALUE)))
             :arity [0 1] :lazy? true)

(f/register! "AREAS"
  ;; Number of disjoint areas in the reference. For a single ref/area, 1.
  ;; Multi-area refs would come from the `:union` shape from the parser.
             (fn [args]
               (let [v (first args)]
                 (cond
                   (= :union (:t v)) (val/number (double (count (:parts v))))
                   :else             (val/number 1.0))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; CELL / SHEET / INFO / SINGLE
;;
;; CELL(info_type, [reference]) returns metadata about a cell. We read
;;   from the workbook in ctx so "row" / "col" / "address" / "contents"
;;   / "type" / "filename" / "sheetname" all return sensible values.
;; SHEET(value) returns the 1-based sheet index of a reference or sheet
;;   name. No arg returns the current sheet.
;; INFO(type) returns workbook-level metadata ("numfile", "recalc", etc.).

(defn- col-letters [^long c]
  ;; Use rechentafel.address/col-idx->letters which is already cljc-clean
  ;; (handles A-Z, AA-ZZ, etc. via codepoint string fns).
  (addr/col-idx->letters c))

(defn- ref-sheet-row-col
  "Pull [sheet-idx row col] out of a :ref AST node or an evaluated value.
  Returns nil if `x` carries no cell identity."
  [ctx x]
  (cond
    (and (map? x) (= :ref (:op x)))
    (let [wb  (:wb ctx)
          si  (cond
                (:sheet-idx x) (:sheet-idx x)
                (:sheet x)     (get (:sheet-names wb) (:sheet x))
                :else          (:cur-sheet wb 0))]
      [si (long (:row x)) (long (:col x))])
    (and (map? x) (= :range (:op x)))
    (recur ctx (:left x))
    :else nil))

(defn- cell-type-code
  "Excel CELL('type',…) codes: 'b' blank, 'l' text/label, 'v' value."
  [v]
  (cond (val/blank? v) "b"
        (val/str? v)   "l"
        :else          "v"))

(f/register! "CELL"
             (fn [ctx ast-args]
               (let [info-arg (eval1 ctx (first ast-args))
                     info     (when (val/str? info-arg)
                                (str/lower-case (:v info-arg)))
          ;; 2nd arg: raw AST if given, else use :current-cell as a
          ;; synthetic ref node.
                     ref-ast  (if (>= (count ast-args) 2)
                                (second ast-args)
                                (when-let [cur (:current-cell ctx)]
                                  {:op :ref :row (:row cur) :col (:col cur)
                                   :sheet-idx (:sheet cur)}))
                     triple   (ref-sheet-row-col ctx ref-ast)]
                 (cond
                   (nil? info)    val/ERR-VALUE
                   (nil? triple)  val/ERR-VALUE
                   :else
                   (let [[si r c] triple
                         wb  (:wb ctx)
                         v   (when (and si (nat-int? r) (nat-int? c)
                                        (fn? (:cell-value ctx)))
                               ((:cell-value ctx) si r c))
                         sheet-name (some (fn [[n i]] (when (= i si) n))
                                          (:sheet-names wb))]
                     (case info
                       "address"  (val/string (str "$" (col-letters c) "$" (inc r)))
                       "col"      (val/number (double (inc c)))
                       "row"      (val/number (double (inc r)))
                       "contents" (or v val/BLANK)
                       "type"     (val/string (cell-type-code (or v val/BLANK)))
                       "filename" (val/string (or (get-in ctx [:wb :filename]) ""))
                       "sheetname" (val/string (or sheet-name ""))
                       "width"    (val/number 10.0)
                       "color"    (val/number 0.0)
                       "parentheses" (val/number 0.0)
                       "prefix"   (val/string "")
                       "protect"  (val/number 1.0)
                       "format"   (val/string "G")
                       val/ERR-VALUE)))))
             :arity [1 2] :lazy? true :volatile? true)

(f/register! "SHEET"
             (fn [ctx ast-args]
               (if (empty? ast-args)
                 (val/number (double (inc (long (get-in ctx [:wb :cur-sheet] 0)))))
                 (let [a (first ast-args)
                       v (eval1 ctx a)
                       wb (:wb ctx)]
                   (cond
          ;; explicit :ref with sheet qualifier
                     (and (map? a) (= :ref (:op a)) (:sheet a))
                     (if-let [i (get (:sheet-names wb) (:sheet a))]
                       (val/number (double (inc i)))
                       val/ERR-REF)
          ;; :range with sheet qualifier on left
                     (and (map? a) (= :range (:op a)) (get-in a [:left :sheet]))
                     (if-let [i (get (:sheet-names wb) (get-in a [:left :sheet]))]
                       (val/number (double (inc i)))
                       val/ERR-REF)
          ;; string literal — look up by name
                     (val/str? v)
                     (if-let [i (get (:sheet-names wb) (:v v))]
                       (val/number (double (inc i)))
                       val/ERR-NA)
          ;; unqualified ref — current sheet
                     :else
                     (val/number (double (inc (long (get-in ctx [:wb :cur-sheet] 0)))))))))
             :arity [0 1] :lazy? true)

(f/register! "SHEETS"
             (fn [ctx ast-args]
               (let [wb (:wb ctx)]
                 (if (empty? ast-args)
                   (val/number (double (count (:sheet-names wb))))
                   (let [a (first ast-args)]
                     (cond
            ;; 3D range like Sheet1:Sheet3!A1
                       (and (map? a) (= :range (:op a)) (get-in a [:left :last-sheet]))
                       (let [s0 (get (:sheet-names wb) (get-in a [:left :sheet]))
                             s1 (get (:sheet-names wb) (get-in a [:left :last-sheet]))]
                         (if (and s0 s1)
                           (val/number (double (inc (Math/abs (- (long s0) (long s1))))))
                           val/ERR-REF))
                       :else (val/number 1.0))))))
             :arity [0 1] :lazy? true)

(f/register! "INFO"
             (fn [args]
               (let [t (first args)
                     os-name #?(:clj (or (System/getProperty "os.name") "")
                                :cljs "")
                     os-ver  #?(:clj (or (System/getProperty "os.version") "")
                                :cljs "")
                     totmem  #?(:clj (.totalMemory (Runtime/getRuntime))
                                :cljs 0.0)]
                 (if-not (val/str? t) val/ERR-VALUE
                         (case (str/lower-case (:v t))
                           "directory"  (val/string "")
                           "numfile"    (val/number 1.0)
                           "origin"     (val/string "$A:$A$1")
                           "osversion"  (val/string os-ver)
                           "recalc"     (val/string "Automatic")
                           "release"    (val/string "16.0")
                           "system"     (val/string (cond (str/starts-with? os-name "Mac") "mac"
                                                          :else "pcdos"))
                           "totmem"     (val/number (double totmem))
                           val/ERR-VALUE))))
             :arity [1 1] :volatile? true)

(f/register! "SINGLE"
  ;; @-style implicit-intersection: reduce a 1xN or Nx1 area to a scalar.
             (fn [args]
               (let [v (first args)]
                 (cond
                   (val/area? v) (val/first-cell-of v)
                   :else         v)))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Formula introspection (Excel 2013+).
;;
;; ISFORMULA(ref): TRUE if the referenced cell contains a formula.
;; We inspect the raw AST to find the ref target, then ask the workbook
;; whether there's a formula stored at that cell id.

(f/register! "ISOMITTED"
  ;; LAMBDA companion: returns TRUE iff the parameter was omitted at the
  ;; call site (bound to the OMITTED sentinel by `apply-lambda`).
             (fn [args]
               (val/boolean-v (val/omitted? (first args))))
             :arity [1 1])

(f/register! "ISFORMULA"
             (fn [ctx ast-args]
               (let [a (first ast-args)
                     triple (ref-sheet-row-col ctx a)]
                 (if (nil? triple)
                   val/ERR-REF
                   (let [[si r c] triple
                         wb (:wb ctx)
                         id (when (and si (nat-int? r) (nat-int? c))
                              (cell/pack (long si) (long r) (long c)))]
                     (val/boolean-v (boolean (and id (contains? (:formulas wb) id))))))))
             :arity [1 1] :lazy? true)

;; FORMULATEXT(ref): if the referenced cell contains a formula, return
;; its source as a string (with leading "="). Otherwise #N/A. Uses the
;; rc-normalised AST stored in :formulas, then resolves it back to live
;; coordinates so the rendered formula matches what the user sees.

(f/register! "FORMULATEXT"
             (fn [ctx ast-args]
               (let [a (first ast-args)
                     triple (ref-sheet-row-col ctx a)]
                 (if (nil? triple)
                   val/ERR-NA
                   (let [[si r c] triple
                         wb (:wb ctx)
                         id (when (and si (nat-int? r) (nat-int? c))
                              (cell/pack (long si) (long r) (long c)))
                         rc-ast (when id (get-in wb [:formulas id]))]
                     (if rc-ast
                       (val/string (str "=" (unparse/unparse (rc/resolve-at rc-ast r c))))
                       val/ERR-NA)))))
             :arity [1 1] :lazy? true)
