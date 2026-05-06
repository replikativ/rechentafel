(ns rechentafel.eval
  "Evaluator + workbook state for the v2 engine.

  A workbook bundles:
    :sheets       — vector of MTV sheets (typed column storage)
    :sheet-names  — name → sheet index
    :cur-sheet    — default sheet context for unqualified refs
    :formulas     — cell-id → AST (formula cells only)
    :deps         — cell-id → #{cell-id it depends on}
    :rdeps        — cell-id → #{cell-id that depends on it}
    :dirty        — set of cell-ids pending recompute
    :volatile     — set of cell-ids that dirty on every recalc
                    (NOW / TODAY / RAND / OFFSET / INDIRECT / ...)

  Edits (`set-cell`) update the graph synchronously; `recalc` walks the
  dirty set in topo order and writes results back to the MTV sheets.

  Dep tracking is the \"broadcaster-ranges\" style: ranges expand to the
  set of constituent cell-ids. Simple, but fat for full-column refs —
  later we'll track range edges directly (LibreOffice-style)."
  (:require [clojure.string      :as str]
            [rechentafel.mtv          :as mtv]
            [rechentafel.cell         :as cell]
            [rechentafel.platform     :as p]
            [rechentafel.rng          :as rng]
            [rechentafel.value        :as val]
            [rechentafel.parser       :as parser]
            [rechentafel.parser-cache :as parser-cache]
            [rechentafel.rc           :as rc]
            [rechentafel.functions    :as functions]))

;; ---------------------------------------------------------------------------
;; Workbook constructors / accessors

;; Sector size for range-edge dependency buckets. A formula reading
;; A1:A100 writes one sector-rdep entry per sector its range touches,
;; not one per cell. Whole-column refs are tolerable because a 1M-row
;; column of sector-bucket-size SZ produces (1M / SZ) entries.
(def ^:const ^:private sector-bits 6) ;; 64x64 — ~16k cells per sector
(def ^:const ^:private sector-size 64)

(defn- sec-r ^long [^long r] (bit-shift-right r sector-bits))
(defn- sec-c ^long [^long c] (bit-shift-right c sector-bits))

(defn- range-sectors
  "Return a lazy seq of [sheet sec-r sec-c] sector keys the given range
  touches. 3D ranges (with `:sheet-end` set) emit one sector per sheet
  in [sheet..sheet-end]."
  [{:keys [sheet sheet-end r0 r1 c0 c1]}]
  (let [s0  (long sheet)
        s1  (long (or sheet-end sheet))
        sr0 (sec-r (long r0)) sr1 (sec-r (long r1))
        sc0 (sec-c (long c0)) sc1 (sec-c (long c1))]
    (for [s  (range s0 (inc s1))
          sr (range sr0 (inc sr1))
          sc (range sc0 (inc sc1))]
      [s sr sc])))

(defn- range-contains-cell? [{:keys [sheet sheet-end r0 r1 c0 c1]}
                             ^long cell-sheet ^long r ^long c]
  (and (<= (long sheet) cell-sheet (long (or sheet-end sheet)))
       (<= (long r0) r (long r1))
       (<= (long c0) c (long c1))))

(defn empty-workbook
  ([] (empty-workbook ["Sheet1"]))
  ([sheet-names]
   {:sheets      (vec (repeat (count sheet-names) (mtv/empty-sheet)))
    :sheet-names (into {} (map-indexed (fn [i n] [n i])) sheet-names)
    :cur-sheet   0
    ;; :formulas stores the R1C1-normalised AST per cell; identical
    ;; formulas (e.g. =A1*2 filled down a column) share the exact same
    ;; object via :shared-asts interning.
    :formulas    {}
    :shared-asts {}
    ;; Defined names. Excel names are case-insensitive, so keys are
    ;; upper-cased. Value is the target AST (a :ref, :range, or any
    ;; formula AST). A future-proof scope key would be [:global name] /
    ;; [sheet-idx name] — we stick to :global for now.
    :names       {}
    ;; Range-edge dep index:
    ;;   :reads        {formula-id → #{{:sheet :r0 :r1 :c0 :c1} …}}
    ;;   :sector-rdeps {[sheet sr sc] → #{formula-id …}}
    ;; Avoids the broadcaster-range explosion (100-cell range → 100 rdep
    ;; entries). Cell invalidation walks sectors, then precision-filters.
    :reads        {}
    :sector-rdeps {}
    :dirty       #{}
    :volatile    #{}
    ;; Excel tables (ListObjects). Keyed case-insensitively. Each
    ;; entry's :ref is the full extent including header and totals
    ;; rows; :header-rows and :totals-rows tell the resolver how to
    ;; compute the data band. Populated by `define-table` or by the
    ;; POI loader reading xl/tables/tableN.xml.
    :tables      {}
    ;; Dynamic-array spill registry. anchor-id → {:r0 :c0 :r1 :c1
    ;; :error nil-or-:blocked}. Set by recalc when a formula returns
    ;; an :area; sibling cells in [r0..r1, c0..c1] hold the
    ;; element values directly in :sheets, no separate :formulas
    ;; entry. The `A1#` operator reads :spills to find the live shape.
    :spills      {}}))

(defn define-table
  "Register an Excel table (ListObject) on the workbook.

  `spec` is a map with at least:
    :sheet         sheet index (or sheet name)
    :ref           [r0 c0 r1 c1] full extent inclusive
    :columns       [\"Col1\" \"Col2\" ...]   ;; order matters
  Optional:
    :header-rows   0 or 1 (default 1)
    :totals-rows   0 or 1 (default 0)

  Names are case-insensitive on lookup."
  [wb ^String name spec]
  (let [sheet-key (:sheet spec)
        sheet-idx (cond
                    (integer? sheet-key) (long sheet-key)
                    (string? sheet-key)  (or (get (:sheet-names wb) sheet-key)
                                             (throw (ex-info "unknown sheet"
                                                             {:sheet sheet-key})))
                    :else (throw (ex-info "missing :sheet" {:spec spec})))
        cols      (vec (:columns spec))
        col-index (into {} (map-indexed (fn [i c] [(str/upper-case c) i]))
                        cols)
        meta {:name        name
              :sheet       sheet-idx
              :ref         (vec (:ref spec))
              :header-rows (long (:header-rows spec 1))
              :totals-rows (long (:totals-rows spec 0))
              :columns     cols
              :column-index col-index}]
    (assoc-in wb [:tables (str/upper-case name)] meta)))

(defn- resolve-table
  "Look up table metadata case-insensitively. Returns nil if not found."
  [wb ^String name]
  (when name
    (get (:tables wb) (str/upper-case name))))

(defn define-name
  "Register a defined name. `target` may be a string (`\"A1:A10\"` or
  `\"=SUM(A1:A10)\"` — leading `=` optional) or a pre-parsed AST. The
  name is stored case-insensitively; resolution happens at eval time."
  [wb ^String nm target]
  (let [ast (cond
              (and (map? target) (:op target)) target
              (string? target)
              (parser-cache/parse (if (str/starts-with? target "=")
                                    (subs target 1) target))
              :else (throw (ex-info "bad name target" {:target target})))]
    (assoc-in wb [:names (str/upper-case nm)] ast)))

(defn- resolve-name
  "Look up a defined name. Returns the AST or nil."
  [wb ^String nm]
  (get (:names wb) (str/upper-case nm)))

(defn- intern-ast
  "Intern an R1C1-form AST. Returns [wb' ast'] where ast' is either a
  freshly-installed value or the existing interned one for this hash."
  [wb rc-ast]
  (let [h (rc/hash-of rc-ast)
        existing (get-in wb [:shared-asts h])]
    (if (and existing (= existing rc-ast))
      [wb existing]
      [(assoc-in wb [:shared-asts h] rc-ast) rc-ast])))

(defn- ast-has-volatile?
  "Walks an AST looking for any :call whose target is registered as volatile.
  Also flags :range with whole-col/row refs since those are handled through
  dynamic-range fns (OFFSET/INDIRECT). Called at formula install time."
  [ast]
  (case (:op ast)
    :call    (or (some-> (:name ast) functions/lookup :volatile? boolean)
                 (boolean (some ast-has-volatile? (:args ast))))
    :binop   (or (ast-has-volatile? (:left ast))
                 (ast-has-volatile? (:right ast)))
    :unop    (ast-has-volatile? (:arg ast))
    :postop  (ast-has-volatile? (:arg ast))
    :union   (boolean (some ast-has-volatile? (:args ast)))
    :intersect (or (ast-has-volatile? (:left ast))
                   (ast-has-volatile? (:right ast)))
    :array   (boolean (some (fn [row] (some ast-has-volatile? row)) (:rows ast)))
    :single-cell (ast-has-volatile? (:arg ast))
    :spill-ref   true                  ;; live-shape lookup is volatile-ish
    :let     (or (boolean (some (fn [[_ v]] (ast-has-volatile? v)) (:bindings ast)))
                 (ast-has-volatile? (:body ast)))
    :lambda  (ast-has-volatile? (:body ast))
    :lambda-call (or (ast-has-volatile? (:fn ast))
                     (boolean (some ast-has-volatile? (:args ast))))
    false))

(defn- sheet-idx
  "Resolve ref-node's :sheet qualifier to an integer index. Returns nil
  for an unknown sheet name — callers treat that as #REF!. With no
  qualifier, falls back to the current sheet."
  [wb ref-node]
  (if-let [s (:sheet ref-node)]
    (get (:sheet-names wb) s)
    (long (:cur-sheet wb 0))))

(defn- ref->id [wb r]
  (when-let [s (sheet-idx wb r)]
    (cell/pack (long s) (long (:row r)) (long (:col r)))))

(defn cell-value
  "Read a tagged value from the workbook by cell id. During a recalc
  pass, in-flight writes live in :pending (map of id→value) — check
  there first so downstream formulas in topo order see fresh values
  before the bulk flush."
  [wb ^long id]
  (or (when-let [p (:pending wb)] (get p id))
      (let [s (cell/sheet id)
            sheet (get (:sheets wb) s)]
        (if sheet
          (mtv/sheet-get sheet (cell/row id) (cell/col id))
          val/BLANK))))

(defn- put-cell-value [wb ^long id v]
  (let [s (cell/sheet id)
        r (cell/row id)
        c (cell/col id)
        sheet (get (:sheets wb) s (mtv/empty-sheet))]
    (assoc-in wb [:sheets s] (mtv/sheet-put sheet r c v))))

;; ---------------------------------------------------------------------------
;; Dep collection — walk AST and gather RANGE SHAPES this formula reads.
;; A single-cell ref is a 1x1 range. Whole-col/row refs become sheet-wide
;; ranges. The sector index makes even A:A cheap — ~1000 sectors for a
;; 1M-row column, not 1M rdep entries.

(def ^:private max-row 1048575)
(def ^:private max-col 16383)

(defn- last-sheet-idx
  "If the ref carries a :last-sheet (3D Sheet1:Sheet3 form), resolve it
  to a sheet index. Returns nil for unknown sheet names. For non-3D
  refs returns nil."
  [wb r]
  (when-let [end (:last-sheet r)]
    (get (:sheet-names wb) end)))

(defn- ref->range [wb r]
  (when-let [s (sheet-idx wb r)]
    (let [whole (:whole r)
          se    (or (last-sheet-idx wb r) s)
          rng   (case whole
                  :col   {:sheet s :r0 0 :r1 max-row
                          :c0 (long (:col r 0)) :c1 (long (:col r 0))}
                  :row   {:sheet s :r0 (long (:row r 0)) :r1 (long (:row r 0))
                          :c0 0 :c1 max-col}
                  (let [row (long (:row r 0)) col (long (:col r 0))]
                    {:sheet s :r0 row :r1 row :c0 col :c1 col}))]
      (cond-> rng
        (not= s se) (assoc :sheet-end (long (max s se))
                           :sheet (long (min s se)))))))

(defn- pair->range [wb left right]
  (when-let [s (sheet-idx wb left)]
    (let [lw (:whole left) rw (:whole right)
          se (or (last-sheet-idx wb left) s)
          r0 (cond
               (or (= :col lw) (= :col rw)) 0
               :else (long (min (:row left 0) (:row right 0))))
          r1 (cond
               (or (= :col lw) (= :col rw)) max-row
               :else (long (max (:row left 0) (:row right 0))))
          c0 (cond
               (or (= :row lw) (= :row rw)) 0
               :else (long (min (:col left 0) (:col right 0))))
          c1 (cond
               (or (= :row lw) (= :row rw)) max-col
               :else (long (max (:col left 0) (:col right 0))))]
      (cond-> {:sheet s :r0 r0 :r1 r1 :c0 c0 :c1 c1}
        (not= s se) (assoc :sheet-end (long (max s se))
                           :sheet (long (min s se)))))))

;; ---------------------------------------------------------------------------
;; Structured (table) reference resolution.
;;
;; Lex/parse hand us a `:table-ref` AST node with a list of specifier
;; items. We collapse the items into one row-band + one column-band and
;; project against the table's stored extent. Resolution happens at
;; eval time (and dep-collection time) so FORMULATEXT can re-emit the
;; original `Sales[Amount]` source rather than a materialised A1 range.

(defn- area-codes
  "Extract the set of #area keywords from a specifier list."
  [specs]
  (reduce (fn [acc s] (if (= :area (:kind s)) (conj acc (:value s)) acc))
          #{} specs))

(defn- column-spec
  "Find the (single) column or column-range specifier in the list, if any."
  [specs]
  (some (fn [s]
          (case (:kind s)
            (:column :column-range) s
            nil))
        specs))

(defn- resolve-table-row-band
  "Given a table's full extent + the specifiers, return [r0 r1] for the
  rows requested. Returns nil for invalid combinations like
  [#Totals] when the table has no totals row."
  [{[fr0 _ fr1 _] :ref hr :header-rows tr :totals-rows} specs current-cell]
  (let [hr     (long hr)
        tr     (long tr)
        fr0    (long fr0)
        fr1    (long fr1)
        data-r0 (+ fr0 hr)
        data-r1 (- fr1 tr)
        codes  (area-codes specs)]
    (cond
      ;; #This Row collapses to current cell's row inside the data band
      (contains? codes :this-row)
      (when-let [r (:row current-cell)]
        (when (<= data-r0 (long r) data-r1) [(long r) (long r)]))

      ;; #All — full extent including header + totals
      (contains? codes :all) [fr0 fr1]

      ;; combinations of #Headers / #Data / #Totals
      (or (contains? codes :headers)
          (contains? codes :data)
          (contains? codes :totals))
      (let [r0 (cond (contains? codes :headers) fr0
                     (contains? codes :data)    data-r0
                     :else                      fr1)
            r1 (cond (contains? codes :totals) fr1
                     (contains? codes :data)   data-r1
                     :else                     (+ fr0 hr -1))]
        (when (<= r0 r1) [r0 r1]))

      ;; default: data band only
      :else
      (when (<= data-r0 data-r1) [data-r0 data-r1]))))

(defn- resolve-table-col-band
  "Given a table and an optional column specifier, return [c0 c1]."
  [{[_ fc0 _ fc1] :ref ci :column-index} cspec]
  (let [fc0 (long fc0)
        fc1 (long fc1)]
    (cond
      (nil? cspec) [fc0 fc1]
      (= :column (:kind cspec))
      (when-let [i (get ci (str/upper-case (:name cspec)))]
        [(+ fc0 (long i)) (+ fc0 (long i))])
      (= :column-range (:kind cspec))
      (when-let [a (get ci (str/upper-case (:from cspec)))]
        (when-let [b (get ci (str/upper-case (:to cspec)))]
          (let [lo (min (long a) (long b))
                hi (max (long a) (long b))]
            [(+ fc0 lo) (+ fc0 hi)]))))))

(defn- table-ref->range
  "Resolve a `:table-ref` AST node to a range descriptor
  `{:sheet :r0 :r1 :c0 :c1}`, or nil if the table or its specifiers
  don't resolve. `current-cell` is the row/col of the formula being
  evaluated, used for `[#This Row]` and the `[@col]` shorthand."
  [wb ast current-cell]
  (when-let [t (resolve-table wb (:table ast))]
    (let [specs (:specifiers ast)
          rows  (resolve-table-row-band t specs current-cell)
          cols  (resolve-table-col-band t (column-spec specs))]
      (when (and rows cols)
        (let [[r0 r1] rows
              [c0 c1] cols]
          {:sheet (long (:sheet t))
           :r0 (long r0) :r1 (long r1)
           :c0 (long c0) :c1 (long c1)})))))

(declare range->area)

(defn collect-reads
  "Walk AST and collect the set of range-shapes the formula reads.
  Whole-col/row refs become full-sheet ranges — cheap under the sector
  index. `env` is the lexical environment (set of bound names that
  shadow workbook defined names). Phase-0 plumbing: env is currently
  always empty; LET will populate it."
  ([wb ast] (collect-reads wb #{} ast))
  ([wb env ast] (persistent! (collect-reads wb env ast (transient #{}))))
  ([wb env ast acc]
   (case (:op ast)
     :num    acc
     :str    acc
     :bool   acc
     :err    acc
     :missing acc
     :ref    (if-let [r (ref->range wb ast)] (conj! acc r) acc)
     :range  (if-let [r (pair->range wb (:left ast) (:right ast))]
               (conj! acc r) acc)
     :name   (if (contains? env (:value ast))
               acc
               (if-let [t (resolve-name wb (:value ast))]
                 (collect-reads wb env t acc) acc))
     :union  (reduce (fn [a x] (collect-reads wb env x a)) acc (:args ast))
     :intersect (-> (collect-reads wb env (:left ast) acc)
                    (->> (collect-reads wb env (:right ast))))
     :call   (reduce (fn [a x] (collect-reads wb env x a)) acc (:args ast))
     :binop  (-> (collect-reads wb env (:left ast) acc)
                 (->> (collect-reads wb env (:right ast))))
     :unop   (collect-reads wb env (:arg ast) acc)
     :postop (collect-reads wb env (:arg ast) acc)
     :array  (reduce (fn [a row]
                       (reduce (fn [a2 x] (collect-reads wb env x a2)) a row))
                     acc (:rows ast))
     :single-cell (collect-reads wb env (:arg ast) acc)
     ;; A1# reads the anchor cell PLUS the spill rectangle. Adding
     ;; just the anchor cell as a 1x1 range gives us re-eval when the
     ;; anchor changes; the spill cells are also tracked because
     ;; consumers reading sibling coords will hit them via existing
     ;; read tracking when sibling values change.
     :spill-ref (if-let [r (ref->range wb (:anchor ast))]
                  (conj! acc r) acc)
     :table-ref (if-let [r (table-ref->range wb ast functions/*current-cell*)]
                  (conj! acc r) acc)
     ;; LET: each binding's value is read in the env at that point.
     ;; Names bound by earlier pairs shadow workbook names in later
     ;; pairs and in the body — `LET(A1, 5, A1)` adds no read of cell
     ;; A1, just like `:name` lookup at eval time.
     :let    (let [[acc env']
                   (reduce (fn [[a e] [n v]]
                             [(collect-reads wb e v a)
                              (conj e (str/upper-case n))])
                           [acc env]
                           (:bindings ast))]
               (collect-reads wb env' (:body ast) acc))
     ;; LAMBDA: collect reads from the body, shadowing parameter names.
     ;; The captured env doesn't matter here — that's an eval-time
     ;; concern; for dep tracking we only care which workbook cells
     ;; could be read.
     :lambda (let [env' (reduce (fn [e p] (conj e (str/upper-case (:name p))))
                                env (:params ast))]
               (collect-reads wb env' (:body ast) acc))
     :lambda-call (let [acc (collect-reads wb env (:fn ast) acc)]
                    (reduce (fn [a x] (collect-reads wb env x a)) acc (:args ast)))
     acc)))

;; ---------------------------------------------------------------------------
;; Evaluator — AST → tagged value.

(declare eval-ast)

(defn- eval-ref [wb ast]
  (if (:last-sheet ast)
    ;; 3D single-cell ref like Sheet1:Sheet3!A1 — materialise as a 3D
    ;; area (one 1x1 slab per sheet in the range). Aggregate fns like
    ;; SUM iterate every slab; non-aggregates see :area3d and error.
    (if-let [r (ref->range wb ast)]
      (range->area wb r)
      val/ERR-REF)
    (if-let [id (ref->id wb ast)]
      (cell-value wb id)
      val/ERR-REF)))

(defn- sheet-bounds
  "Actual populated extent of a sheet as [max-row max-col]. Whole-col/
  whole-row ranges clip to these, so =SUM(A:A) iterates only populated
  rows instead of a million blanks."
  [wb ^long s]
  (let [sheet (get (:sheets wb) s)]
    (if (empty? sheet)
      [0 0]
      [(long (reduce max 0 (keep :max-row sheet)))
       (long (dec (count sheet)))])))

(defn- materialise-slab
  "Read one [r0..r1, c0..c1] block off `sheet` as a vec-of-vec of
  tagged values, with whole-col / whole-row clipping baked in."
  [wb s ref-r0 ref-r1 ref-c0 ref-c1]
  (let [s      (long s)
        ref-r0 (long ref-r0) ref-r1 (long ref-r1)
        ref-c0 (long ref-c0) ref-c1 (long ref-c1)
        [mr mc] (sheet-bounds wb s)
        whole? (or (= ref-r1 max-row) (= ref-c1 max-col))
        r1     (long (if whole? (min ref-r1 mr) ref-r1))
        c1     (long (if whole? (min ref-c1 mc) ref-c1))]
    (cond
      (or (> ref-r0 r1) (> ref-c0 c1))
      [[val/BLANK]]
      :else
      (vec (for [r (range ref-r0 (inc r1))]
             (vec (for [c (range ref-c0 (inc c1))]
                    (cell-value wb (cell/pack s r c)))))))))

(defn- sheet-name-of [wb ^long s]
  (some (fn [[n i]] (when (= i s) n)) (:sheet-names wb)))

(defn- range->area
  "Materialise a `{:sheet :r0 :r1 :c0 :c1 [:sheet-end]}` range descriptor
  into an `:area` (2D) or `:area3d` (sheet range) tagged value. Whole-col
  / whole-row ranges are clipped to each sheet's populated extent."
  [wb rng]
  (let [s0     (long (:sheet rng))
        s1     (long (:sheet-end rng s0))
        ref-r0 (long (:r0 rng))
        ref-r1 (long (:r1 rng))
        ref-c0 (long (:c0 rng))
        ref-c1 (long (:c1 rng))
        whole? (or (= ref-r1 max-row) (= ref-c1 max-col))]
    (if (= s0 s1)
      (let [vals (materialise-slab wb s0 ref-r0 ref-r1 ref-c0 ref-c1)
            r1*  (dec (+ ref-r0 (count vals)))
            c1*  (dec (+ ref-c0 (count (first vals))))]
        (cond-> {:t :area :sheet (sheet-name-of wb s0)
                 :r0 ref-r0 :c0 ref-c0 :r1 r1* :c1 c1*
                 :values vals}
          whole? (assoc :ref-r0 ref-r0 :ref-r1 ref-r1
                        :ref-c0 ref-c0 :ref-c1 ref-c1)))
      ;; 3D — one slab per sheet
      (let [slabs (vec (for [s (range s0 (inc s1))]
                         (materialise-slab wb s ref-r0 ref-r1 ref-c0 ref-c1)))
            sheet-names (vec (for [s (range s0 (inc s1))] (sheet-name-of wb s)))]
        (cond-> {:t :area3d
                 :sheet0 s0 :sheet1 s1
                 :sheet-names sheet-names
                 :r0 ref-r0 :c0 ref-c0 :r1 ref-r1 :c1 ref-c1
                 :slabs slabs}
          whole? (assoc :ref-r0 ref-r0 :ref-r1 ref-r1
                        :ref-c0 ref-c0 :ref-c1 ref-c1))))))

(defn- eval-range [wb ast]
  (if-let [rng (pair->range wb (:left ast) (:right ast))]
    (range->area wb rng)
    val/ERR-REF))

(defn- eval-table-ref [wb ast]
  (if-let [rng (table-ref->range wb ast functions/*current-cell*)]
    (let [r0 (long (:r0 rng)) r1 (long (:r1 rng))
          c0 (long (:c0 rng)) c1 (long (:c1 rng))]
      (if (and (= r0 r1) (= c0 c1))
        (cell-value wb (cell/pack (long (:sheet rng)) r0 c0))
        (range->area wb rng)))
    val/ERR-REF))

;; Excel comparison semantics:
;;   =, <>   — strict by type; no cross-type coercion. Blank coerces to
;;             0 (vs :num) and "" (vs :str). Strings compare case-insensitively.
;;   <, <=, >, >= — same-type normal ordering; cross-type uses
;;             num < str < bool ordinal.

(defn- blank-eq? [a b]
  (case (:t b)
    :num   (zero? (double (:v b)))
    :str   (= "" (:v b))
    :blank true
    :bool  (false? (:v b))
    false))

(defn- excel-eq [a b]
  (cond
    (= (:t a) (:t b))
    (case (:t a)
      :num   (= (double (:v a)) (double (:v b)))
      :str   (= (str/lower-case (or (:v a) "")) (str/lower-case (or (:v b) "")))
      :bool  (= (:v a) (:v b))
      :blank true
      (= (:v a) (:v b)))
    (= :blank (:t a)) (blank-eq? a b)
    (= :blank (:t b)) (blank-eq? b a)
    :else false))

(def ^:private type-ordinal {:num 0 :str 1 :bool 2 :blank -1 :err 99})

(defn- excel-cmp ^long [a b]
  (if (= (:t a) (:t b))
    (case (:t a)
      :num   (compare (double (:v a)) (double (:v b)))
      :str   (compare (str/lower-case (or (:v a) ""))
                      (str/lower-case (or (:v b) "")))
      :bool  (compare (if (:v a) 1 0) (if (:v b) 1 0))
      :blank 0
      0)
    (compare (long (type-ordinal (:t a) 0))
             (long (type-ordinal (:t b) 0)))))

(defn- apply-binop-scalar
  "Apply a binop to two *scalar* (non-area) operands. Errors propagate.
  Called per-cell from `lift-binop` for area operands."
  [sym left right]
  (cond
    (val/err? left)  left
    (val/err? right) right
    :else
    (case sym
      :plus  (val/number (+ (functions/num! left) (functions/num! right)))
      :minus (val/number (- (functions/num! left) (functions/num! right)))
      :mul   (val/number (* (functions/num! left) (functions/num! right)))
      :div   (let [r (functions/num! right)]
               (if (zero? r) val/ERR-DIV0
                   (val/number (/ (functions/num! left) r))))
      :pow   (val/number (Math/pow (functions/num! left) (functions/num! right)))
      :concat (val/string (str (functions/str! left) (functions/str! right)))
      :eq (val/boolean-v (excel-eq left right))
      :ne (val/boolean-v (not (excel-eq left right)))
      :lt (val/boolean-v (neg? (excel-cmp left right)))
      :le (val/boolean-v (not (pos? (excel-cmp left right))))
      :gt (val/boolean-v (pos? (excel-cmp left right)))
      :ge (val/boolean-v (not (neg? (excel-cmp left right))))
      val/ERR-VALUE)))

(defn- safe-binop
  "Top-level binop dispatch with broadcasting over areas. Either side
  can be a scalar or an area; broadcasting follows Excel's row/col
  vector replication rules in `functions/lift-binop`. Errors at the
  scalar level propagate per cell."
  [sym left right]
  (try
    (functions/lift-binop (fn [l r] (apply-binop-scalar sym l r))
                          left right)
    (catch #?(:clj Throwable :cljs :default) e
      (or (some-> (ex-data e) :excel-error val/error)
          val/ERR-VALUE))))

(defn- lazy-ctx [wb env]
  {:wb wb
   :env env
   :current-cell functions/*current-cell*
   :eval         (fn [ctx ast] (eval-ast wb (:env ctx env) ast))
   :parse        (fn [s] (parser/parse s))
   :cell-value   (fn [s r c] (cell-value wb (cell/pack s r c)))
   :resolve-area (fn [_ctx coords]
                   ;; OFFSET/INDIRECT build their own range; resolve it
                   ;; into an :area value or a scalar cell value.
                   (let [s  (if-let [n (:sheet coords)]
                              (or (get (:sheet-names wb) n) (:cur-sheet wb 0))
                              (:cur-sheet wb 0))
                         r0 (long (:r0 coords))
                         r1 (long (:r1 coords))
                         c0 (long (:c0 coords))
                         c1 (long (:c1 coords))]
                     (cond
                       (or (neg? r0) (neg? c0) (< r1 r0) (< c1 c0))
                       val/ERR-REF
                       (and (= r0 r1) (= c0 c1))
                       (cell-value wb (cell/pack s r0 c0))
                       :else
                       {:t :area :sheet (:sheet coords)
                        :r0 r0 :c0 c0 :r1 r1 :c1 c1
                        :values
                        (vec (for [r (range r0 (inc r1))]
                               (vec (for [c (range c0 (inc c1))]
                                      (cell-value wb (cell/pack s r c))))))})))})

;; LAMBDA recursion cap matches Excel's empirical ~1024-stack-slot
;; limit (community-derived, since Microsoft doesn't publish a number).
;; Beyond this depth we surface #NUM!, same as Excel.
(def ^:dynamic ^:private *lambda-depth* 0)
(def ^:const ^:private MAX-LAMBDA-DEPTH 1024)

(declare apply-lambda eval-ast)

(defn call-lambda-with-values
  "Apply a `:lambda` value to a list of already-evaluated tagged
  values. Used by LAMBDA helpers (MAP, REDUCE, BYROW, ...) which
  iterate over arrays and want to invoke the lambda per cell without
  going back through AST eval. Returns #VALUE! on arity mismatch,
  #NUM! when recursion exceeds Excel's ~1024 cap."
  [wb {:keys [params body env] :as _lambda} arg-values]
  (let [n-given (count arg-values)
        n-req   (count (filter (complement :optional?) params))
        n-max   (count params)]
    (cond
      (>= (long *lambda-depth*) (long MAX-LAMBDA-DEPTH))
      val/ERR-NUM
      (or (< n-given n-req) (> n-given n-max))
      val/ERR-VALUE
      :else
      (let [all-vals (vec (concat arg-values
                                  (repeat (- n-max n-given) val/OMITTED)))
            env'     (reduce (fn [e [p v]]
                               (assoc e (str/upper-case (:name p)) v))
                             env (map vector params all-vals))]
        (binding [*lambda-depth* (inc (long *lambda-depth*))]
          (eval-ast wb env' body))))))

(defn- apply-lambda
  "Apply a `:lambda` value to a list of caller-side AST args. Each arg
  is evaluated in the *caller's* env; values flow through
  `call-lambda-with-values`."
  [wb caller-env lambda arg-asts]
  (let [vals (mapv (fn [ast]
                     (if (= :missing (:op ast))
                       val/OMITTED
                       (eval-ast wb caller-env ast)))
                   arg-asts)]
    (call-lambda-with-values wb lambda vals)))

(defn- eval-call [wb env ast]
  (let [fname  (:name ast)
        up     (str/upper-case fname)
        ;; Excel resolves defined names BEFORE the function table —
        ;; `SUM = LAMBDA(...)` shadows the builtin SUM. We follow that
        ;; rule, but only when the resolved binding is actually a
        ;; lambda value; non-lambda defined names fall through so a
        ;; range alias `Total = A1:A10` doesn't break `=SUM(Total)`.
        env-bound (get env up)
        named-ast (resolve-name wb fname)
        named-val (cond
                    (val/lambda? env-bound) env-bound
                    (some? named-ast) (let [v (eval-ast wb env named-ast)]
                                        (when (val/lambda? v) v)))
        reg       (functions/lookup fname)]
    (cond
      named-val
      (apply-lambda wb env named-val (:args ast))

      reg
      (if (:lazy? reg)
        (functions/call-lazy (lazy-ctx wb env) fname (:args ast))
        (functions/call fname (mapv #(eval-ast wb env %) (:args ast))))

      ;; Bound via LET / non-lambda defined name — calling a value that
      ;; isn't a lambda is #VALUE!.  Otherwise the name is unknown.
      (or (some? env-bound) (some? named-ast))
      val/ERR-VALUE

      :else
      val/ERR-NAME)))

(defn eval-ast
  "Evaluate an AST under the given workbook + lexical environment.
  `env` is a map of bound name → tagged value, populated by LET and
  LAMBDA application; empty for top-level cell evaluation."
  ([wb ast] (eval-ast wb {} ast))
  ([wb env ast]
   (case (:op ast)
     :num     (val/number (:value ast))
     :str     (val/string (:value ast))
     :bool    (val/boolean-v (:value ast))
     :err     (val/error (:value ast))
     :missing val/BLANK
     :ref     (eval-ref wb ast)
     :range   (eval-range wb ast)
     :call    (eval-call wb env ast)
     :binop   (safe-binop (:sym ast)
                          (eval-ast wb env (:left ast))
                          (eval-ast wb env (:right ast)))
     :unop    (let [a (eval-ast wb env (:arg ast))]
                (functions/lift-call
                 (fn [[v]]
                   (cond
                     (val/err? v) v
                     :else (case (:sym ast)
                             :minus (val/number (- (functions/num! v)))
                             :plus  (val/to-num v))))
                 [a]))
     :postop  (let [a (eval-ast wb env (:arg ast))]
                (functions/lift-call
                 (fn [[v]]
                   (cond
                     (val/err? v) v
                     :else (case (:sym ast)
                             :percent (val/number (/ (functions/num! v) 100.0)))))
                 [a]))
     ;; `@` — implicit-intersection / single-cell operator. Reduces a
     ;; range/area/array to one scalar using the formula cell's row/col
     ;; (legacy Excel rule via `functions/implicit-intersect`). Modern
     ;; engine fires this only when the user wrote `@` explicitly or
     ;; when an arg slot is tagged scalar-only.
     :single-cell (functions/implicit-intersect (eval-ast wb env (:arg ast)))
     ;; `A1#` — spill-range operator (ANCHORARRAY). Resolves through
     ;; wb[:spills] to the live shape of the anchor cell's spill;
     ;; returns the materialised area, or #REF! if the anchor isn't
     ;; currently a spill anchor.
     :spill-ref (let [a       (:anchor ast)
                      sheet   (sheet-idx wb a)
                      anchor-id (when sheet
                                  (cell/pack (long sheet)
                                             (long (:row a))
                                             (long (:col a))))
                      spill   (when anchor-id
                                (get (:spills wb) anchor-id))]
                  (cond
                    (nil? spill)   val/ERR-REF
                    (:error spill) (val/error :spill)
                    :else (range->area wb {:sheet  (long sheet)
                                           :r0 (long (:r0 spill))
                                           :c0 (long (:c0 spill))
                                           :r1 (long (:r1 spill))
                                           :c1 (long (:c1 spill))})))
     :name    (or (get env (str/upper-case (:value ast)))
                  (if-let [t (resolve-name wb (:value ast))]
                    (eval-ast wb env t)
                    val/ERR-NAME))
     :table-ref (eval-table-ref wb ast)
     :let     (let [env' (reduce (fn [e [n v]]
                                   (assoc e (str/upper-case n)
                                          (eval-ast wb e v)))
                                 env (:bindings ast))]
                (eval-ast wb env' (:body ast)))
     :lambda  {:t :lambda
               :params (:params ast)
               :body   (:body ast)
               :env    env}
     :lambda-call (let [f (eval-ast wb env (:fn ast))]
                    (cond
                      (val/err? f) f
                      (val/lambda? f) (apply-lambda wb env f (:args ast))
                      :else val/ERR-VALUE))
     :union   (eval-ast wb env (first (:args ast)))
     :intersect val/ERR-NA
     :array   (let [rows (mapv (fn [row] (mapv #(eval-ast wb env %) row))
                               (:rows ast))]
                {:t :area :r0 0 :c0 0
                 :r1 (dec (count rows))
                 :c1 (dec (count (first rows)))
                 :values rows})
     val/ERR-VALUE)))

;; ---------------------------------------------------------------------------
;; Dep-graph maintenance — sector-bucketed reverse-dependencies

(defn- remove-reads [wb ^long id]
  (let [old-reads (get (:reads wb) id)]
    (if (seq old-reads)
      (-> wb
          (update :reads dissoc id)
          (update :sector-rdeps
                  (fn [srd]
                    (reduce (fn [srd rng]
                              (reduce (fn [srd sec]
                                        (let [s (disj (get srd sec) id)]
                                          (if (empty? s)
                                            (dissoc srd sec)
                                            (assoc srd sec s))))
                                      srd (range-sectors rng)))
                            srd old-reads))))
      (update wb :reads dissoc id))))

(defn- add-reads [wb ^long id new-reads]
  (if (seq new-reads)
    (-> wb
        (assoc-in [:reads id] new-reads)
        (update :sector-rdeps
                (fn [srd]
                  (reduce (fn [srd rng]
                            (reduce (fn [srd sec]
                                      (update srd sec (fnil conj #{}) id))
                                    srd (range-sectors rng)))
                          srd new-reads))))
    wb))

(defn- formulas-reading-cell
  "Which formulas read the given cell? Walks the sector the cell belongs
  to, then precision-filters each candidate against its stored ranges."
  [wb ^long cell-id]
  (let [cs (cell/sheet cell-id)
        cr (cell/row cell-id)
        cc (cell/col cell-id)
        sec [(long cs) (sec-r cr) (sec-c cc)]
        candidates (get (:sector-rdeps wb) sec)]
    (when (seq candidates)
      (filter (fn [fid]
                (some #(range-contains-cell? % cs cr cc)
                      (get (:reads wb) fid)))
              candidates))))

(defn- transitive-dirty
  "Starting from `seed` cells, walk the reverse-dependency graph and
  return every formula-id whose transitive inputs include a seed cell."
  [wb seed]
  (loop [dirty #{} stack (vec seed)]
    (if (empty? stack)
      dirty
      (let [cid (peek stack)
            readers (formulas-reading-cell wb cid)
            fresh   (reduce (fn [acc f]
                              (if (contains? dirty f) acc (conj acc f)))
                            [] readers)]
        (recur (into dirty fresh) (into (pop stack) fresh))))))

(defn- spill-anchors-touching
  "Return the set of spill anchor-ids whose rectangle contains
  `(sheet, row, col)`. Used by set-cell to mark spill anchors dirty
  when a cell inside their rectangle (sibling, intended sibling for a
  blocked anchor, or the anchor itself) is edited."
  [wb ^long sheet ^long row ^long col]
  (set (for [[anchor-id {:keys [r0 c0 r1 c1]}] (:spills wb)
             :when (and (= sheet (cell/sheet anchor-id))
                        (<= (long r0) row (long r1))
                        (<= (long c0) col (long c1)))]
         anchor-id)))

;; ---------------------------------------------------------------------------
;; Public edit API

(defn- formula-input? [input]
  (and (string? input) (str/starts-with? input "=")))

(defn set-cell
  "Set a cell. `input` can be:
    - a Clojure number / string / boolean / nil  → literal
    - a tagged-value map (has :t)                → literal
    - a leading-`=` string                       → formula
    - a pre-parsed AST map (has :op)             → formula AST
  Returns an updated workbook with this cell (and its transitive
  downstream) marked dirty."
  [wb ^long id input]
  (let [[ast literal-v]
        (cond
          (and (map? input) (:op input)) [input nil]
          (and (map? input) (:t input))  [nil input]
          (formula-input? input)
          [(parser-cache/parse (subs input 1)) nil]
          (number? input)   [nil (val/number input)]
          (string? input)   [nil (val/string input)]
          (boolean? input)  [nil (val/boolean-v input)]
          (nil? input)      [nil val/BLANK]
          :else             [nil (val/string (str input))])]
    ;; Spill awareness: an edit anywhere inside an anchor's spill
    ;; rectangle (sibling cell or the would-be cells of a #SPILL!
    ;; anchor) re-dirties the anchor so it can re-spill / re-attempt.
    ;; Computed against the workbook's current spill state, before
    ;; we touch :reads / :formulas.
    (let [touched-anchors (spill-anchors-touching wb (cell/sheet id)
                                                  (cell/row id)
                                                  (cell/col id))]
      (if ast
        (let [row     (cell/row id)
              col     (cell/col id)
              rc-ast  (rc/normalize ast row col)
              [wb interned] (intern-ast wb rc-ast)
              live-ast (rc/resolve-at interned row col)
              new-reads (collect-reads wb live-ast)
              vol?     (ast-has-volatile? interned)
              wb      (-> wb
                          (remove-reads id)
                          (add-reads id new-reads)
                          (assoc-in [:formulas id] interned)
                          (update :volatile (if vol? #(conj % id) #(disj % id)))
                          (update :dirty
                                  (fn [d] (-> d (conj id)
                                              (into touched-anchors)
                                              (into (transitive-dirty wb [id]))))))]
          wb)
        (-> wb
            (remove-reads id)
            (update :formulas dissoc id)
            (update :volatile disj id)
            (put-cell-value id literal-v)
            (update :dirty
                    (fn [d] (-> d
                                (into touched-anchors)
                                (into (transitive-dirty wb [id]))))))))))

;; ---------------------------------------------------------------------------
;; Recompute — topo-sort the dirty set (restricted to formulas) and eval.

(defn- dirty-formulas [wb]
  (filter #(contains? (:formulas wb) %) (:dirty wb)))

(defn- flush-pending
  "Fold pending writes into the MTV sheets, grouping by (sheet, col) so
  contiguous numeric runs land in one block. Non-numeric / non-blank
  values fall through the per-cell path (can't share a block type)."
  [sheets pending]
  ;; Group writes by [sheet col] → sorted list of [row value].
  (let [by-col (reduce (fn [acc [id v]]
                         (update acc [(cell/sheet id) (cell/col id)]
                                 (fnil conj []) [(cell/row id) v]))
                       {} pending)
        by-col (into {} (map (fn [[k vs]] [k (sort-by first vs)])) by-col)]
    (reduce
     (fn [sheets [[s c] writes]]
       (let [sheet (get sheets s (mtv/empty-sheet))
             ;; Split into runs of consecutive rows with the same tag
             ;; (:num / :blank / other). :num runs flush as a bulk
             ;; primitive range; others flush single-cell.
             sheet'
             (loop [ws writes sheet sheet]
               (if (empty? ws) sheet
                   (let [[row v] (first ws)
                         tag (:t v)
                         ;; accumulate same-tag contiguous run
                         [run rest]
                         (loop [r row out [v] more (rest ws)]
                           (if-let [[nr nv] (first more)]
                             (if (and (= nr (inc r))
                                      (= (:t nv) tag))
                               (recur nr (conj out nv) (next more))
                               [out more])
                             [out nil]))
                         start row
                         n     (count run)]
                     (cond
                       (and (= tag :num) (> n 1))
                       (let [a (p/make-num-array n)]
                         (dotimes [i n] (aset a i (double (:v (nth run i)))))
                         (recur rest
                                (mtv/sheet-put-num-col sheet c start a)))
                       (= tag :blank)
                       (recur rest
                              (reduce (fn [sh i] (mtv/sheet-put sh (+ start i) c val/BLANK))
                                      sheet (range n)))
                       :else
                       (recur rest
                              (reduce (fn [sh [rr vv]]
                                        (mtv/sheet-put sh rr c vv))
                                      sheet
                                      (map vector (range start (+ start n)) run)))))))]
         (assoc sheets s sheet')))
     sheets
     by-col)))

(def ^:private ^:const small-range-threshold 32)

(defn- range-cell-count ^long [{:keys [r0 r1 c0 c1]}]
  (* (inc (- (long r1) (long r0)))
     (inc (- (long c1) (long c0)))))

(defn- predecessors-in-nodes
  "Formulas inside `node-set` that `formula-id` depends on — i.e. nodes
  whose owner-cell lies inside one of `formula-id`'s read-ranges. Picks
  the cheaper iteration: for small ranges, enumerate cells and hit-test
  against node-set; for large ones, iterate node-set and range-test.
  This flips topo-sort on linear chains from O(N²) to O(N)."
  [wb formula-id node-set]
  (let [reads (get (:reads wb) formula-id)
        n-nodes (count node-set)]
    (when (seq reads)
      (persistent!
       (reduce
        (fn [acc rng]
          (let [s  (long (:sheet rng))
                r0 (long (:r0 rng))
                r1 (long (:r1 rng))
                c0 (long (:c0 rng))
                c1 (long (:c1 rng))]
            (if (< (range-cell-count rng)
                   (max small-range-threshold n-nodes))
              (loop [acc acc r r0]
                (if (> r r1) acc
                    (recur
                     (loop [acc acc c c0]
                       (if (> c c1) acc
                           (let [cid (cell/pack s r c)]
                             (recur (if (and (not= cid (long formula-id))
                                             (contains? node-set cid))
                                      (conj! acc cid)
                                      acc)
                                    (inc c)))))
                     (inc r))))
              (reduce (fn [acc n]
                        (let [^long n n]
                          (if (and (not= n (long formula-id))
                                   (range-contains-cell? rng
                                                         (cell/sheet n)
                                                         (cell/row n)
                                                         (cell/col n)))
                            (conj! acc n)
                            acc)))
                      acc node-set))))
        (transient [])
        reads)))))

(defn- topo-order
  "Kahn's algorithm over the sub-dep-graph induced by `nodes`. Returns a
  vector in evaluation order. Cycles break by arbitrary tie-break — the
  remaining-in-cycle nodes are appended and later marked #REF!."
  [wb nodes]
  (let [node-set (set nodes)
        in-deps  (into {} (for [n nodes]
                            [n (vec (predecessors-in-nodes wb n node-set))]))
        in-count (into {} (for [[n ds] in-deps] [n (count ds)]))
        rev      (reduce-kv (fn [acc n ds]
                              (reduce (fn [a d] (update a d (fnil conj []) n))
                                      acc ds))
                            {} in-deps)]
    (loop [ready (into #?(:clj  clojure.lang.PersistentQueue/EMPTY
                          :cljs cljs.core/PersistentQueue.EMPTY)
                       (for [[n c] in-count :when (zero? c)] n))
           in-count in-count
           out []]
      (if (empty? ready)
        (if (= (count out) (count nodes))
          {:order out :cycle #{}}
          {:order out :cycle (set (for [[n c] in-count :when (pos? c)] n))})
        (let [n (peek ready)
              ready (pop ready)
              children (get rev n)
              [in-count ready]
              (reduce (fn [[ic rq] c]
                        (let [v (dec (long (get ic c)))]
                          [(assoc ic c v)
                           (if (zero? v) (conj rq c) rq)]))
                      [in-count ready]
                      children)]
          (recur ready in-count (conj out n)))))))

;; ---------------------------------------------------------------------------
;; Spill resolution (M2 of array support).
;;
;; When a formula evaluates to an :area, we materialise its cells into
;; the surrounding rectangle (siblings) and record the shape in
;; wb[:spills][anchor-id]. The `A1#` operator reads that registry to
;; pick up the live shape. If any prospective sibling slot is occupied
;; by something other than this anchor's prior siblings, we anchor a
;; #SPILL! error and leave the conflicting cells alone.

(defn- spill-cells
  "Yield [id row col] for every sibling slot in the rectangle defined
  by anchor + shape. Includes the anchor itself."
  [anchor-id rows cols]
  (let [s  (cell/sheet anchor-id)
        r0 (cell/row anchor-id)
        c0 (cell/col anchor-id)]
    (for [r (range r0 (+ r0 (long rows)))
          c (range c0 (+ c0 (long cols)))]
      [(cell/pack s r c) r c])))

(defn- prior-siblings
  "Cell-ids that this anchor previously spilled into (excluding the
  anchor itself), per wb[:spills]. Returns #{} when no prior spill."
  [wb anchor-id]
  (if-let [{:keys [r0 c0 r1 c1]} (get (:spills wb) anchor-id)]
    (let [s (cell/sheet anchor-id)]
      (set (for [r (range (long r0) (inc (long r1)))
                 c (range (long c0) (inc (long c1)))
                 :let [id (cell/pack s r c)]
                 :when (not= id anchor-id)]
             id)))
    #{}))

(defn- pending-or-cell [pending wb id]
  (or (when pending (get pending id))
      (cell-value wb id)))

(defn- spill-blocked?
  "Check if any sibling slot is occupied by a cell that isn't part of
  this anchor's prior spill. Returns the blocking ids, or nil."
  [wb pending anchor-id rows cols prior-set]
  (let [bad
        (for [[id _ _] (spill-cells anchor-id rows cols)
              :when (and (not= id anchor-id)
                         (not (contains? prior-set id))
                         (or (contains? (:formulas wb) id)
                             (let [v (pending-or-cell pending wb id)]
                               (not (val/blank? v)))))]
          id)]
    (when (seq bad) bad)))

(defn- materialise-spill
  "Write each cell of the area into pending, return [pending wb']
  with the new spill shape recorded in wb'. Caller is responsible
  for clearing prior siblings outside the new shape."
  [wb pending anchor-id area]
  (let [rows (inc (- (long (:r1 area)) (long (:r0 area))))
        cols (inc (- (long (:c1 area)) (long (:c0 area))))
        s    (cell/sheet anchor-id)
        ar0  (cell/row anchor-id)
        ac0  (cell/col anchor-id)
        vals (:values area)
        pending'
        (loop [pending pending r 0]
          (if (>= r rows) pending
              (recur (loop [pending pending c 0]
                       (if (>= c cols) pending
                           (let [v   (get-in vals [r c] val/BLANK)
                                 id  (cell/pack s (+ ar0 r) (+ ac0 c))]
                             (recur (assoc! pending id v) (inc c)))))
                     (inc r))))
        wb'  (assoc-in wb [:spills anchor-id]
                       {:r0 ar0 :c0 ac0
                        :r1 (+ ar0 rows -1) :c1 (+ ac0 cols -1)
                        :error nil})]
    [pending' wb']))

(defn- clear-prior-siblings-outside
  "Write BLANK to pending for any prior sibling not in the new shape."
  [wb pending anchor-id rows cols]
  (let [s        (cell/sheet anchor-id)
        ar0      (cell/row anchor-id)
        ac0      (cell/col anchor-id)
        new-r1   (+ ar0 (long rows) -1)
        new-c1   (+ ac0 (long cols) -1)
        priors   (prior-siblings wb anchor-id)
        survivors (set (for [[id _ _] (spill-cells anchor-id rows cols)
                             :when (and (not= id anchor-id)
                                        (contains? priors id))]
                         id))]
    (reduce (fn [pending id]
              (if (contains? survivors id)
                pending
                (assoc! pending id val/BLANK)))
            pending priors)))

(defn- handle-spill
  "Top-level dispatch when a formula evaluates to an :area. Returns
  [pending wb'] with the spill (or #SPILL!) recorded."
  [wb pending anchor-id area]
  (let [rows  (inc (- (long (:r1 area)) (long (:r0 area))))
        cols  (inc (- (long (:c1 area)) (long (:c0 area))))
        prior (prior-siblings wb anchor-id)
        block (spill-blocked? wb pending anchor-id rows cols prior)]
    (cond
      ;; 1x1 area collapses to a scalar — no siblings to write.
      (and (= 1 rows) (= 1 cols))
      (let [pending (clear-prior-siblings-outside wb pending anchor-id 1 1)
            v       (get-in (:values area) [0 0] val/BLANK)
            wb'     (update wb :spills dissoc anchor-id)]
        [(assoc! pending anchor-id v) wb'])

      block
      ;; Record the *intended* rectangle on a blocked anchor so a
      ;; later edit anywhere in that rect can trigger re-evaluation.
      (let [pending (clear-prior-siblings-outside wb pending anchor-id rows cols)
            ar0     (cell/row anchor-id)
            ac0     (cell/col anchor-id)
            wb'     (assoc-in wb [:spills anchor-id]
                              {:r0 ar0 :c0 ac0
                               :r1 (+ ar0 rows -1) :c1 (+ ac0 cols -1)
                               :error :blocked})]
        [(assoc! pending anchor-id (val/error :spill)) wb'])

      :else
      (let [pending (clear-prior-siblings-outside wb pending anchor-id rows cols)
            [pending wb'] (materialise-spill wb pending anchor-id area)]
        [pending wb']))))

(declare recalc-impl)

(defn recalc
  "Evaluate every dirty formula in dep order and write results back to
  the sheets. Seeds the dirty set with the volatile set first so that
  NOW/TODAY/RAND/OFFSET/INDIRECT-bearing formulas re-eval every pass,
  and their transitive downstream comes along.

  When `wb[:rng-seed]` is set, RAND-family fns draw from a fresh PRNG
  seeded from it for the duration of this recalc — same seed always
  produces the same sequence."
  [wb]
  (binding [rng/*rng* (when-let [seed (:rng-seed wb)]
                        (rng/make (long seed)))]
    (recalc-impl wb)))

(defn- recalc-impl [wb]
  (let [wb     (update wb :dirty
                       (fn [d]
                         (let [seeded (into d (:volatile wb))]
                           (into seeded (transitive-dirty wb (:volatile wb))))))
        nodes  (vec (dirty-formulas wb))
        {:keys [order cycle]} (topo-order wb nodes)
        ;; Two-phase recalc:
        ;;   1. Walk topo order, eval each formula against wb + :pending
        ;;      (a transient map of in-flight writes). Writes don't touch
        ;;      the MTV yet — they land in :pending so downstream cells
        ;;      read fresh values cheaply. Spill anchors update wb's
        ;;      :spills field as we go so consumer formulas reading via
        ;;      the `A1#` operator see the live shape.
        ;;   2. Flush :pending into :sheets in one bulk per-column pass.
        pending0 (transient {})
        [pending wb]
        (reduce (fn [[pending wb] ^long id]
                  (let [rc-ast (get-in wb [:formulas id])
                        row    (cell/row id)
                        col    (cell/col id)
                        ast    (rc/resolve-at rc-ast row col)
                        wb'    (assoc wb :pending pending)
                        v   (binding [functions/*current-cell*
                                      {:sheet (cell/sheet id)
                                       :row   row
                                       :col   col}]
                              (eval-ast wb' ast))]
                    (let [;; Unwrap 1x1 areas to scalars at the cell-write
                          ;; boundary. A 1x1 :area is a degenerate spill
                          ;; that should land in storage as the single
                          ;; tagged value, matching how Excel renders a
                          ;; spill of size 1.
                          v (if (and (= :area (:t v))
                                     (= (long (:r0 v)) (long (:r1 v)))
                                     (= (long (:c0 v)) (long (:c1 v))))
                              (get-in (:values v) [0 0] val/BLANK)
                              v)]
                      (cond
                        (= :area (:t v))
                        (handle-spill wb pending id v)

                        ;; Scalar — if anchor previously spilled, clear
                        ;; those siblings.
                        (contains? (:spills wb) id)
                        (let [pending (clear-prior-siblings-outside wb pending id 1 1)
                              wb'     (update wb :spills dissoc id)]
                          [(assoc! pending id v) wb'])

                        :else
                        [(assoc! pending id v) wb]))))
                [pending0 wb] order)
        pending  (reduce #(assoc! %1 %2 val/ERR-REF) pending cycle)
        sheets   (flush-pending (:sheets wb) (persistent! pending))]
    (assoc wb :sheets sheets :dirty #{})))

;; ---------------------------------------------------------------------------
;; Convenience

(defn get-cell
  "Read the current (possibly stale if :dirty non-empty) value of a cell."
  ([wb ^long id] (cell-value wb id))
  ([wb sheet ^long row ^long col]
   (cell-value wb (cell/pack (long (or (get (:sheet-names wb) sheet) sheet)) row col))))

(defn set-and-recalc
  "Convenience: set one cell and trigger a full recalc."
  [wb ^long id input]
  (recalc (set-cell wb id input)))
