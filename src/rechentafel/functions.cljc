(ns rechentafel.functions
  "Registry + dispatch helpers for Excel formula functions.

  A function is registered under its uppercase name with a map:
    {:fn       <implementation>
     :arity    [min max]      ; inclusive arg-count bounds, e.g. [1 1], [0 30]
     :lazy?    false          ; true: receives (ctx ast-args); false: (values)
     :volatile? false         ; NOW/TODAY/RAND/INDIRECT/OFFSET/CELL/INFO
     :array?   false}         ; SUMPRODUCT / MMULT / array-aware

  Strict impls are `(fn [args] -> value)`. Lazy impls receive the evaluator
  context and the un-evaluated AST args so they can decide which branches
  to evaluate; their implementation lives with the evaluator, but the
  registration lives alongside the strict fns for a single source of
  truth.

  Dispatch happens through `call` below, which:
    - Looks the fn up (case-insensitively)
    - Validates arity
    - Short-circuits on first error arg (strict only)
    - Invokes the impl, catching domain errors and turning them into
      Excel error values so evaluation can continue.

  Values use the tagged-map shape from `rechentafel.value` (`:num`/`:str`/
  `:bool`/`:blank`/`:err`/`:ref`/`:area`)."
  (:refer-clojure :exclude [peek])
  (:require [clojure.string :as str]
            [rechentafel.platform :as p]
            [rechentafel.value :as val]))

;; ---------------------------------------------------------------------------
;; Registry — plain atom around an immutable map. Modules `register!` their
;; fns at load time; callers only ever read via `lookup` / `call`.

(defonce ^:private *registry (atom {}))

(defn register!
  "Install a function under its uppercase name. `opts` may set:
    :arity      [min max]
    :lazy?      true → impl receives (ctx ast-args), false → (values)
    :volatile?  true → re-evaluated every recalc (NOW/RAND/...)
    :array?     true → consumes whole areas (legacy flag, kept for
                       compatibility with code that checks it)
    :lift?      true → call wrapper element-wise iterates area args;
                       impl always sees scalars. Set on pure-scalar fns
                       (SQRT, ABS, etc.) to enable broadcasting.
                       Default false: impl receives areas as-is (this
                       is the legacy behaviour and matches the wide
                       majority of registered fns, which iterate areas
                       themselves via walk-scalars).

  Calling `register!` twice for the same name overwrites — the last module
  to load wins. Modules should not collide; fail loudly if they do."
  [fname f & {:keys [arity lazy? volatile? array? lift?]
              :or   {lazy? false volatile? false array? false}}]
  (let [k (str/upper-case (name fname))
        ;; If `:lift?` not explicitly set, honour the fn's `:scalar?`
        ;; metadata — pure-scalar helpers (n1, n2, ...) attach it so
        ;; broadcasting works without each register! call repeating
        ;; the flag. Default false otherwise.
        lift?  (cond
                 (some? lift?) lift?
                 (:scalar? (meta f)) true
                 :else false)]
    (swap! *registry assoc k
           (cond-> {:fn f :name k :lift? lift?}
             arity     (assoc :arity arity)
             lazy?     (assoc :lazy? true)
             volatile? (assoc :volatile? true)
             array?    (assoc :array? true)))
    k))

(defn lookup
  "Returns the registration map for `fname`, or nil. Case-insensitive."
  [fname]
  (get @*registry (str/upper-case (name fname))))

(defn registered-names
  "All registered function names, sorted."
  []
  (sort (keys @*registry)))

(defn count-registered [] (count @*registry))

;; ---------------------------------------------------------------------------
;; Arity handling

(defn- arity-ok?
  "`arity` is [min max]; either endpoint may be nil for unbounded."
  [arity n-args]
  (or (nil? arity)
      (let [[mn mx] arity]
        (and (or (nil? mn) (>= n-args mn))
             (or (nil? mx) (<= n-args mx))))))

;; ---------------------------------------------------------------------------
;; Error short-circuit
;;
;; POI scans args left-to-right; the first error encountered becomes the
;; function's result. This is how `IFERROR(A1, ...)` catches errors from
;; A1 rather than having them swallow the whole formula.
;;
;; Applies to STRICT functions only — lazy functions choose when to eval
;; each arg and therefore when to honour errors themselves.

(defn- first-error [args]
  (some (fn [a] (when (val/err? a) a)) args))

;; ---------------------------------------------------------------------------
;; call — the single entry point

(declare lift-call area?)

(defn call
  "Invoke the registered fn with `args` (already-evaluated values). Returns
  a value — either the fn's result or an Excel error. If the fn is not
  registered, returns #NAME?. If arity is wrong, returns #VALUE!. If any
  arg is an error, that error is returned unchanged (strict semantics).

  Functions registered with `:lift? true` (the default) and called with
  any area argument get element-wise iteration via `lift-call` — the
  fn sees scalars only and the wrapper builds an area result."
  [fname args]
  (if-let [{f :fn arity :arity lift? :lift?} (lookup fname)]
    (cond
      (not (arity-ok? arity (count args))) val/ERR-VALUE
      :else
      (or (first-error args)
          (try
            (if (and lift? (some area? args))
              (lift-call f args)
              (f args))
            (catch #?(:clj Throwable :cljs :default) e
              (or (some-> (ex-data e) :excel-error val/error)
                  val/ERR-VALUE)))))
    val/ERR-NAME))

(defn call-lazy
  "Invoke a registered lazy fn with the evaluator context + unevaluated
  AST args. The evaluator calls this when it sees a :call node whose
  target is lazy. The lazy impl is free to skip branches."
  [ctx fname ast-args]
  (if-let [{f :fn arity :arity lazy? :lazy?} (lookup fname)]
    (cond
      (not lazy?)                                 val/ERR-NAME
      (not (arity-ok? arity (count ast-args)))    val/ERR-VALUE
      :else
      (try (f ctx ast-args)
           (catch #?(:clj Throwable :cljs :default) e
             (or (some-> (ex-data e) :excel-error val/error)
                 val/ERR-VALUE))))
    val/ERR-NAME))

;; ---------------------------------------------------------------------------
;; Helpers for implementations
;;
;; These let individual functions stay terse. Usage pattern for a simple
;; 1-arg numeric fn (POI's NumericFunction.ABS style):
;;
;;   (register! "ABS" (n1 #(Math/abs ^double %)) :arity [1 1])

(defn domain-error!
  "Throw an exception that `call` will convert to the given Excel error."
  [code]
  (throw (ex-info (str "domain error: " code) {:excel-error code})))

;; ---------------------------------------------------------------------------
;; Implicit intersection — when a scalar coercer sees an :area value,
;; project it to a single cell using *current-cell* (the row/col of the
;; formula being evaluated). This mirrors Excel's legacy behaviour:
;;   =SIN(A1:A10)  in B2  →  SIN(A2)
;; Cases:
;;   1x1 area        → the sole cell
;;   single row Nx1  → cell in current-cell's column, if inside
;;   single col 1xN  → cell in current-cell's row, if inside
;;   otherwise       → #VALUE!
;; When *current-cell* is not bound (e.g. repl/debug eval), falls back
;; to (0,0) of the area.

(def ^:dynamic *current-cell* nil)

(defn implicit-intersect
  "Project an :area value to a scalar using *current-cell*. Non-areas
  pass through."
  [v]
  (if (= :area (:t v))
    (let [cc   *current-cell*
          r0   (long (:r0 v 0)) r1 (long (:r1 v 0))
          c0   (long (:c0 v 0)) c1 (long (:c1 v 0))
          row  (when cc (long (:row cc)))
          col  (when cc (long (:col cc)))
          vals (:values v)]
      (cond
        (and (= r0 r1) (= c0 c1))
        (get-in vals [0 0])
        (and (= r0 r1) col (<= c0 col c1))
        (get-in vals [0 (- col c0)])
        (and (= c0 c1) row (<= r0 row r1))
        (get-in vals [(- row r0) 0])
        ;; Fallback: first cell if no current-cell bound (e.g. lit tests).
        (nil? cc)
        (get-in vals [0 0])
        :else
        (val/error :value)))
    v))

;; ---------------------------------------------------------------------------
;; Broadcasting (M1 of array support).
;;
;; Excel 365 dynamic-array engine treats most operators and scalar fns
;; as element-wise. We implement that by:
;;   - `lift-binop` — element-wise apply a binop over scalar / area
;;     operands, with row / col vector broadcasting.
;;   - `lift-call` — wrap a strict scalar function so that area args
;;     element-wise iterate; functions registered :lift? false (the
;;     ~30 aggregates / range consumers) bypass this and receive
;;     areas directly.
;;
;; `:area3d` (3D ranges across sheet spans) is intentionally NOT
;; broadcastable: aggregates iterate it via walk-scalars, but binops
;; against it return #VALUE! per Excel's "3D ranges are not allowed"
;; rule in non-aggregate contexts. Same rule applied throughout.

(defn area? [v] (= :area (:t v)))

(defn- area-shape [a]
  [(inc (- (long (:r1 a)) (long (:r0 a))))
   (inc (- (long (:c1 a)) (long (:c0 a))))])

(defn- area-cell [a r c]
  (get-in (:values a) [r c]))

(defn- broadcast-shape
  "Given two values, return [rows cols] of the result, or nil for
  shape-incompatible. A nil dimension on a side means scalar."
  [a b]
  (let [a-area? (area? a)
        b-area? (area? b)]
    (cond
      (not (or a-area? b-area?)) nil
      (and a-area? (not b-area?)) (area-shape a)
      (and b-area? (not a-area?)) (area-shape b)
      :else
      (let [[ra ca] (area-shape a)
            [rb cb] (area-shape b)
            rows (cond (= ra rb)            ra
                       (= 1 ra)             rb
                       (= 1 rb)             ra)
            cols (cond (= ca cb)            ca
                       (= 1 ca)             cb
                       (= 1 cb)             ca)]
        (when (and rows cols) [rows cols])))))

(defn- pick
  "Read one cell from `v` at logical (r, c). For areas with a singleton
  dimension we broadcast (row 0 / col 0). For scalars, just `v`."
  [v r c]
  (cond
    (area? v)
    (let [[rows cols] (area-shape v)
          rr (if (= 1 rows) 0 r)
          cc (if (= 1 cols) 0 c)]
      (area-cell v rr cc))
    :else v))

(defn lift-binop
  "Apply scalar `f` element-wise over `left`/`right`. Either side can
  be a scalar or a 2D area; mismatched non-broadcastable shapes →
  `#VALUE!`. Returns a scalar when both sides are scalars (the `f`
  result), an area otherwise."
  [f left right]
  (cond
    ;; Neither operand is a 2D area — straight scalar binop. (3D
    ;; areas fall through here and `f` will reject them by type.)
    (not (or (area? left) (area? right)))
    (f left right)

    :else
    (if-let [[rows cols] (broadcast-shape left right)]
      (let [vals (vec (for [r (range rows)]
                        (vec (for [c (range cols)]
                               (let [a (pick left  r c)
                                     b (pick right r c)]
                                 (cond
                                   (val/err? a) a
                                   (val/err? b) b
                                   :else (try (f a b)
                                              (catch #?(:clj Throwable :cljs :default) e
                                                (or (some-> (ex-data e) :excel-error val/error)
                                                    val/ERR-VALUE)))))))))]
        {:t :area :r0 0 :c0 0
         :r1 (dec rows) :c1 (dec cols)
         :values vals})
      val/ERR-VALUE)))

(defn- broadcast-shape-of-areas
  "Compute the broadcast shape across multiple areas. Singleton (1)
  dimensions broadcast against any size; mismatched non-singleton
  dimensions return ::mismatch."
  [areas]
  (reduce (fn [[ra ca] a]
            (let [[rb cb] (area-shape a)
                  rows (cond (= ra rb) ra
                             (= 1 ra)  rb
                             (= 1 rb)  ra
                             :else     ::mismatch)
                  cols (cond (= ca cb) ca
                             (= 1 ca)  cb
                             (= 1 cb)  ca
                             :else     ::mismatch)]
              (if (or (= ::mismatch rows) (= ::mismatch cols))
                (reduced ::mismatch)
                [rows cols])))
          (area-shape (first areas))
          (rest areas)))

(defn- lift-cell [f args r c]
  (let [args' (mapv #(pick % r c) args)]
    (if-let [err (first-error args')]
      err
      (try (f args')
           (catch #?(:clj Throwable :cljs :default) e
             (or (some-> (ex-data e) :excel-error val/error)
                 val/ERR-VALUE))))))

(defn lift-call
  "Apply scalar `f` over a list of args. If any arg is an area, lift
  element-wise: the result is an area whose shape is the broadcast of
  all area args; non-area args are scalar-broadcast. Errors propagate
  per cell. Used by the call dispatcher for fns registered :lift? true."
  [f args]
  (let [areas (filter area? args)]
    (if (empty? areas)
      (f args)
      (let [shape (broadcast-shape-of-areas areas)]
        (if (= ::mismatch shape)
          val/ERR-VALUE
          (let [[rows cols] shape
                vals (vec (for [r (range rows)]
                            (vec (for [c (range cols)]
                                   (lift-cell f args r c)))))]
            {:t :area :r0 0 :c0 0
             :r1 (dec rows) :c1 (dec cols)
             :values vals}))))))

(defn num!
  "Coerce a value to a double, or throw a domain error matching POI's
  OperandResolver. If `to-num` returns an error we propagate *that*
  error code (so #DIV/0! stays #DIV/0!); non-number fallbacks become
  #VALUE!."
  ^double [v]
  (let [v (implicit-intersect v)
        n (val/to-num v)]
    (cond
      (val/num? n) (double (:v n))
      (val/err? n) (domain-error! (:v n))
      :else        (domain-error! :value))))

(defn str!
  "Coerce to a string; errors propagate with their specific code."
  ^String [v]
  (let [v (implicit-intersect v)
        s (val/to-str v)]
    (cond
      (val/str? s) (:v s)
      (val/err? s) (domain-error! (:v s))
      :else        (domain-error! :value))))

(defn bool!
  "Coerce to a boolean; errors propagate with their specific code,
  otherwise #VALUE!."
  [v]
  (let [v (implicit-intersect v)
        b (val/to-bool v)]
    (cond
      (val/bool? b) (:v b)
      (val/err? b)  (domain-error! (:v b))
      :else         (domain-error! :value))))

(defn int!
  "Coerce to a long, truncating (Excel's INT-as-coercion style).
  Errors / non-numerics throw domain errors."
  ^long [v]
  (long (num! v)))

(defn check-num!
  "Check a raw double is finite + usable, else throw #NUM!.
  Matches POI's checkValue on NumericFunction."
  ^double [^double x]
  (cond
    (p/nan? x)         (domain-error! :num)
    (not (p/finite? x)) (domain-error! :num)
    :else x))

(defn finite!
  "Coerce to a double and require it to be finite. Combines num! + check-num!."
  ^double [v]
  (check-num! (num! v)))

;; ---------------------------------------------------------------------------
;; Value iteration — the workhorse for aggregation functions.
;;
;; POI's `AggregateFunction.evaluateInternal` walks each arg, expanding
;; area values into their cells, skipping blanks, and selecting numerics
;; or all scalars depending on the fn. We expose two flavours:
;;
;;   (each-scalar args f)         — calls (f v) on every scalar value
;;   (each-numeric args f)        — only numerics (strings coerce per
;;                                  POI's rules: loose for aggregates
;;                                  that accept "1" as 1, strict by
;;                                  default where a non-number string
;;                                  just gets skipped)
;;
;; The `strict-strings?` flag chooses POI's two strategies: true means
;; the function was called with a literal string — treat it as a number
;; if parsable, else #VALUE!. false means the string was inside an area
;; — silently skip. POI mirrors this split in AggregateFunction.

(defn- walk-scalars
  "Call (f scalar in-area?) for every scalar inside `v`. Areas expand to
  their cells (in-area? → true), refs resolve via `:resolved` (if
  present) or are treated as blank. 3D areas (`:area3d` from Sheet1:Sheet3
  ranges) iterate every slab. Errors are passed to `f` like any other
  value — the caller decides whether to propagate them."
  [v f in-area?]
  (case (:t v)
    :area (doseq [row (:values v), cell row]
            (walk-scalars cell f true))
    :area3d (doseq [slab (:slabs v), row slab, cell row]
              (walk-scalars cell f true))
    :ref  (walk-scalars (:resolved v val/BLANK) f in-area?)
    (f v in-area?)))

(defn each-scalar
  "Invoke (f scalar in-area?) for every scalar in `args`. `in-area?` is
  true when the scalar was expanded out of an area/range, false when it
  was a direct argument — matching POI's distinction."
  [args f]
  (doseq [a args] (walk-scalars a f false)))

(defn- parse-num-str [^String s] (p/parse-double s))

(defn sum-numeric
  "Sum the numeric scalars in `args` as a double. POI's AggregateFunction
  semantics: strings inside an area are silently skipped; strings at the
  top level coerce or raise #VALUE!; blanks are skipped; booleans count
  (TRUE=1, FALSE=0); errors propagate as domain errors."
  ^double [args]
  (let [acc (volatile! 0.0)]
    (each-scalar
     args
     (fn [v in-area?]
       (case (:t v)
         :num   (vswap! acc #(+ (double %) (double (:v v))))
         :blank nil
         :bool  (vswap! acc #(+ (double %) (if (:v v) 1.0 0.0)))
         :str   (when-not in-area?
                  (if-let [n (parse-num-str (:v v))]
                    (vswap! acc #(+ (double %) (double n)))
                    (domain-error! :value)))
         :err   (domain-error! (:v v))
         nil)))
    @acc))

(defn count-numeric
  "Count the numeric scalars — POI's COUNT semantics. Numbers always
  count. Booleans and parsable strings count only as top-level args
  (not inside areas). Blanks never count. Errors inside areas are
  skipped; errors at top level propagate."
  ^long [args]
  (let [acc (volatile! 0)]
    (each-scalar
     args
     (fn [v in-area?]
       (case (:t v)
         :num   (vswap! acc inc)
         :bool  (when-not in-area? (vswap! acc inc))
         :str   (when-not in-area?
                  (when (parse-num-str (:v v)) (vswap! acc inc)))
         :err   (when-not in-area? (domain-error! (:v v)))
         nil)))
    @acc))

(defn count-all
  "COUNTA semantics — every non-blank scalar (including errors, strings,
  booleans, numbers)."
  ^long [args]
  (let [acc (volatile! 0)]
    (each-scalar
     args
     (fn [v _] (when-not (val/blank? v) (vswap! acc inc))))
    @acc))

(defn collect-scalars
  "Eager flatten to a persistent vector — used by aggregate fns that need
  random access (MEDIAN, MODE, PERCENTILE). Includes every scalar,
  including errors and blanks; caller filters."
  [args]
  (let [t (volatile! (transient []))]
    (each-scalar args (fn [v _] (vswap! t conj! v)))
    (persistent! @t)))

(defn collect-finite-numerics
  "Aggregate-fn helper: gather numerics into a double array, propagating
  errors, skipping blanks, following POI's string-at-top-level-coerces
  rule. Returns a vector of doubles."
  [args]
  (let [t (volatile! (transient []))]
    (each-scalar
     args
     (fn [v in-area?]
       (case (:t v)
         :num   (vswap! t conj! (double (:v v)))
         :bool  (when-not in-area?
                  (vswap! t conj! (if (:v v) 1.0 0.0)))
         :str   (when-not in-area?
                  (if-let [n (parse-num-str (:v v))]
                    (vswap! t conj! (double n))
                    (domain-error! :value)))
         :err   (domain-error! (:v v))
         nil)))
    (persistent! @t)))

;; ---------------------------------------------------------------------------
;; Module registration barrier
;;
;; Each fn module ends with `(register-all!)`. The aggregator namespace
;; `rechentafel.functions.all` requires all modules so a single
;; `(require 'rechentafel.functions.all)` wires the full registry.
