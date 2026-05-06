(ns rechentafel.parser
  "Precedence-climbing parser for the token stream produced by
  `rechentafel.lexer`. Produces an AST of plain Clojure maps:

    {:op :num   :value 3.14}
    {:op :str   :value \"hi\"}
    {:op :bool  :value true}
    {:op :err   :value :ref}
    {:op :ref   :sheet nil :row 0 :col 0 :abs-row? false :abs-col? false}
    {:op :name  :value \"foo\"}                 ;; defined name or whole-col (A:A)
    {:op :binop :sym :plus :left L :right R}
    {:op :unop  :sym :minus :arg A}
    {:op :postop :sym :percent :arg A}
    {:op :range  :left L :right R}
    {:op :call   :name \"SUM\" :args [...]}
    {:op :array  :rows [[...] [...]]}

  Excel precedence (low → high):
    comparison (= <> <= >= < >) · concat (&) · add/sub · mul/div · pow (^)
    · unary (-+) · postfix (%) · intersection (' ') · range (:)

  Range binds tighter than unary so that `-A1:B5` parses as `-(A1:B5)`, not
  `(-A1):B5`. This is implemented by layering parse-range between
  parse-postfix and parse-atom rather than putting `:` in the binop table.

  Intersection is a whitespace-between-refs operator (POI's IntersectionPtg)
  — `A1:B2 B2:C3` = the 1x1 overlap B2. Lexer annotates tokens with
  `:ws-before?`, and parse-intersection fires when a ref-starting token
  follows a range result with whitespace.

  Commas are argument separators inside function calls. At the top level
  and inside a parenthesised group, commas denote union (POI's UnionPtg) —
  `(A1:B2,B2:C3)` and `$A:$A,$1:$4` both yield `{:op :union :args [...]}`.

  Errors are thrown as `ex-info` with `:type :parse-error` and structured
  data (`:line :col :offset :source :expected :got`). Use `format-error`
  on the ex-data to render a three-line diagnostic with a caret."
  (:require [clojure.string :as str]
            [rechentafel.address :as addr]
            [rechentafel.lexer :as lex]))

(def ^:private binop-prec
  "Higher precedence binds tighter. Range/postfix/unary are handled by the
  dedicated layers below parse-expr and are intentionally absent here."
  {:eq 1 :ne 1 :lt 1 :le 1 :gt 1 :ge 1
   :concat 2
   :plus 3 :minus 3
   :mul 4 :div 4
   :pow 5})

(defn- peek-tok [toks i] (get toks i))
(defn- kind     [toks i] (:kind (get toks i)))

;; ---------------------------------------------------------------------------
;; Error reporting
;;
;; Parse errors carry the source string so `format-error` can render a
;; caret. The source is threaded via a dynamic var rather than as an
;; argument to every parse-* function — parsing is single-threaded and the
;; alternative is thirty prop-drilled signatures for something that's pure
;; diagnostics.

(def ^:dynamic ^:private *src* nil)

(defn- offset->line-col
  "Convert a 0-indexed char offset into 1-indexed [line col]."
  [^String src ^long offset]
  (let [offset (max 0 (min offset (count src)))]
    (loop [i 0, line 1, line-start 0]
      (if (>= i offset)
        [line (inc (- offset line-start))]
        (if (= \newline (get src i))
          (recur (inc i) (inc line) (inc i))
          (recur (inc i) line line-start))))))

(defn- describe-tok
  "Short human-readable name for a token, for error messages."
  [t]
  (if (nil? t)
    "end of input"
    (case (:kind t)
      :num    (str "number " (:value t))
      :str    (str "string " (pr-str (:value t)))
      :bool   (str "boolean " (:value t))
      :err    (str "error #" (name (:value t)))
      :ref    (str "ref " (or (:text t) "?"))
      :ident  (str "name " (pr-str (:value t)))
      :op     (str "operator " (name (:value t)))
      :lparen "'('"
      :rparen "')'"
      :lbrace "'{'"
      :rbrace "'}'"
      :comma  "','"
      :semi   "';'"
      :range  "':'"
      (str "token " (pr-str t)))))

(defn- err!
  "Throw a structured parse error anchored at token index `i`."
  ([toks i expected]
   (err! toks i expected nil))
  ([toks i expected got-override]
   (let [tok (get toks i)
         n   (count (or *src* ""))
         pos (cond
               tok           (:pos tok)
               (pos? n)      n
               :else         0)
         [line col] (offset->line-col (or *src* "") pos)
         got (or got-override (describe-tok tok))
         msg (str "expected " expected " but got " got)]
     (throw (ex-info msg
                     {:type     :parse-error
                      :msg      msg
                      :expected expected
                      :got      got
                      :token    tok
                      :offset   pos
                      :line     line
                      :col      col
                      :source   (or *src* "")})))))

(defn format-error
  "Render parse-error ex-data as a three-line diagnostic:

      Parse error at line L, col C: <msg>
        <offending source line>
               ^

  Pass the map returned by `ex-data` on a caught parse error."
  [{:keys [line col msg source] :as _ex-data}]
  (let [lines    (str/split (or source "") #"\n" -1)
        src-line (nth lines (dec line) "")
        caret    (str (apply str (repeat (dec col) \space)) "^")]
    (str "Parse error at line " line ", col " col ": " msg "\n"
         "  " src-line "\n"
         "  " caret)))

(defn- tok->binop
  "If the token at i is a binary operator, return [op-key prec]."
  [toks i]
  (let [t (peek-tok toks i)]
    (when (= :op (:kind t))
      (when-let [p (binop-prec (:value t))] [(:value t) p]))))

(declare parse-expr parse-prefix)

(def ^:private MISSING {:op :missing})

(defn- parse-args
  "Function-call argument list. i points *inside* the parens, before first
  arg or at `)` for zero-arg calls. Empty slots (leading comma, consecutive
  commas, trailing comma) emit `{:op :missing}` — POI's MissingArgPtg."
  [toks i]
  (if (= :rparen (kind toks i))
    [[] i]
    (loop [args [], i i]
      (let [k         (kind toks i)
            [arg i']  (if (or (= :comma k) (= :rparen k))
                        [MISSING i]
                        (parse-expr toks i 1))
            args      (conj args arg)]
        (case (kind toks i')
          :comma  (recur args (inc i'))
          :rparen [args i']
          (err! toks i' "',' or ')' in argument list"))))))

(defn- parse-array-row [toks i]
  (loop [row [], i i]
    (let [[e i] (parse-expr toks i 1)
          row   (conj row e)
          k     (kind toks i)]
      (cond
        (= k :comma)             (recur row (inc i))
        (or (= k :semi)
            (= k :rbrace))       [row i]
        :else (err! toks i "',', ';' or '}' in array literal")))))

(defn- desugar-let
  "Rewrite `LET(name1, val1, ..., body)` into a `:let` AST. Args must be
  an odd count >= 3 with every odd-positioned arg being a bare :name
  node whose value is a valid identifier (no spaces, not a number).
  Returns nil if the args don't satisfy these constraints — the caller
  falls back to a plain `:call` so eval can surface the Excel error."
  [args]
  (when (and (>= (count args) 3) (odd? (count args)))
    (let [pairs (partition 2 (butlast args))
          body  (last args)]
      (when (every? (fn [[n _]] (and (= :name (:op n))
                                     (let [v (:value n)]
                                       (and (string? v)
                                            (not (re-find #"^\d" v))))))
                    pairs)
        {:op :let
         :bindings (mapv (fn [[n v]] [(:value n) v]) pairs)
         :body body}))))

(defn- lambda-param
  "Recognise a LAMBDA parameter slot. Required params are bare :name
  AST nodes; optional params are written `[Y]` in the source, which
  the lexer tokenises as an implicit-table structured-ref with one
  :column item. Returns `{:name :optional?}` or nil."
  [arg]
  (cond
    (and (= :name (:op arg))
         (string? (:value arg))
         (not (re-find #"^\d" (:value arg))))
    {:name (:value arg) :optional? false}

    (and (= :table-ref (:op arg))
         (nil? (:table arg))
         (= 1 (count (:specifiers arg)))
         (= :column (:kind (first (:specifiers arg)))))
    {:name (:name (first (:specifiers arg))) :optional? true}))

(defn- desugar-lambda
  "Rewrite `LAMBDA(p1, ..., pn, body)` into a `:lambda` AST. All but the
  last arg must be parameter slots; the last is the body. Required
  params must precede optional ones (`[Y]`). Returns nil on malformed
  input — caller falls back to plain :call."
  [args]
  (when (>= (count args) 1)
    (let [param-asts (butlast args)
          body       (last args)
          params     (mapv lambda-param param-asts)]
      (when (and (every? some? params)
                 ;; required before optional
                 (apply <= 0 (map (fn [p] (if (:optional? p) 1 0)) params)))
        {:op :lambda :params (vec params) :body body}))))

(defn- special-form-or-call
  "Decide whether `name(args)` is a regular function call or one of our
  special forms. Falls back to :call so eval can surface #NAME? /
  #VALUE? on malformed input — matches Excel's permissive parse /
  strict eval split."
  [name args]
  (let [up (.toUpperCase ^String name)]
    (case up
      "LET"    (or (desugar-let args)
                   {:op :call :name name :args args})
      "LAMBDA" (or (desugar-lambda args)
                   {:op :call :name name :args args})
      {:op :call :name name :args args})))

(defn- parse-array
  "Array literal {a,b;c,d}. i points *after* the opening `{`."
  [toks i]
  (loop [rows [], i i]
    (let [[row i] (parse-array-row toks i)
          rows    (conj rows row)]
      (case (kind toks i)
        :semi   (recur rows (inc i))
        :rbrace [{:op :array :rows rows} (inc i)]
        (err! toks i "';' or '}' to close array literal")))))

(defn- consume-applications
  "After an atomic expression, consume any chain of `(args)` applications
  (`LAMBDA(x,x)(5)`, `f(1)(2)`) and the `#` spill-range postfix
  (`A1#`). Stops as soon as the next token isn't one of these."
  [node toks i]
  (loop [node node, i i]
    (cond
      (= :lparen (kind toks i))
      (let [[args j] (parse-args toks (inc i))]
        (if (= :rparen (kind toks j))
          (recur {:op :lambda-call :fn node :args args} (inc j))
          (err! toks j "')' to close application")))

      (= :hash (kind toks i))
      ;; `A1#` — spill-range operator. Only valid on a `:ref` AST;
      ;; on anything else, surface a parse error so users notice
      ;; `(1+1)#` rather than silently doing the wrong thing.
      (if (= :ref (:op node))
        (recur {:op :spill-ref :anchor node} (inc i))
        (err! toks i "a cell reference before '#'"))

      :else [node i])))

(defn- parse-atom [toks i]
  (let [t (peek-tok toks i)
        [node j]
        (case (:kind t)
          :num    [{:op :num  :value (:value t)} (inc i)]
          :str    [{:op :str  :value (:value t)} (inc i)]
          :bool   [{:op :bool :value (:value t)} (inc i)]
          :err    [{:op :err  :value (:value t)} (inc i)]
          :ref    [(-> t (dissoc :kind) (assoc :op :ref)) (inc i)]
          :table-ref [(-> t (dissoc :kind) (assoc :op :table-ref)) (inc i)]
          :lparen (let [[e j] (parse-expr toks (inc i) 1)]
                    (case (kind toks j)
                      :rparen [e (inc j)]
                      ;; Commas at the paren-group level mean union (POI's
                      ;; UnionPtg):  `(A1:B2,B2:C3)` → {:op :union :args [...]}
                      :comma  (loop [args [e], j j]
                                (case (kind toks j)
                                  :comma  (let [[arg j] (parse-expr toks (inc j) 1)]
                                            (recur (conj args arg) j))
                                  :rparen [{:op :union :args args} (inc j)]
                                  (err! toks j "',' or ')' in union")))
                      (err! toks j "')' to close parenthesised expression")))
          :lbrace (parse-array toks (inc i))
          :ident  (if (= :lparen (kind toks (inc i)))
                    (let [[args j] (parse-args toks (+ i 2))]
                      (if (= :rparen (kind toks j))
                        [(special-form-or-call (:value t) args) (inc j)]
                        (err! toks j "')' to close function call")))
                    [{:op :name :value (:value t)} (inc i)])
          (err! toks i "an expression"))]
    ;; Tail: chain `(args)` applications (LAMBDA-call or curried fn).
    (consume-applications node toks j)))

;; ---------------------------------------------------------------------------
;; Whole-column / whole-row normalisation
;;
;; `A:A` and `1:1` refer to entire columns / rows. The lexer does not know
;; this (both sides look like a plain ident/num) so we normalise here at
;; range-build time. Shapes we convert:
;;   {:op :name :value "A"}        → whole-col 0
;;   {:op :name :value "$AB"}      → whole-col 27, abs
;;   {:op :num  :value 1.0}        → whole-row 0
;;   {:op :name :value "$3"}       → whole-row 2, abs

(def ^:private letters-only-re #"^(\$?)([A-Za-z]+)$")
(def ^:private digits-only-re  #"^(\$?)(\d+)$")

(defn- as-whole-col
  "Return the node viewed as a whole-column ref, or nil."
  [node]
  (cond
    (and (= :ref (:op node)) (= :col (:whole node)))
    node

    (= :name (:op node))
    (when-let [[_ dollar letters] (re-matches letters-only-re (:value node))]
      {:op :ref
       :whole :col
       :col (addr/col-letters->idx letters)
       :abs-col? (= "$" dollar)})))

(defn- as-whole-row
  "Return the node viewed as a whole-row ref, or nil."
  [node]
  (cond
    (and (= :ref (:op node)) (= :row (:whole node)))
    node

    (and (= :num (:op node))
         (let [v (:value node)] (and (pos? v) (== v (long v)))))
    {:op :ref
     :whole :row
     :row (dec (long (:value node)))
     :abs-row? false}

    (= :name (:op node))
    (when-let [[_ dollar digits] (re-matches digits-only-re (:value node))]
      {:op :ref
       :whole :row
       :row (dec #?(:clj  (Long/parseLong digits)
                    :cljs (js/parseInt digits 10)))
       :abs-row? (= "$" dollar)})))

(defn- propagate-sheet
  "If left carries sheet-qualifier fields and right doesn't, copy them —
  `Sheet1!A1:B5` means both A1 and B5 are in Sheet1; the same applies to
  `Sheet1:Sheet3!A1:B5` (3D) and `[Book]Sheet1!A1:B5` (external)."
  [left right]
  (cond-> right
    (and (:sheet      left) (not (:sheet      right))) (assoc :sheet      (:sheet      left))
    (and (:last-sheet left) (not (:last-sheet right))) (assoc :last-sheet (:last-sheet left))
    (and (:workbook   left) (not (:workbook   right))) (assoc :workbook   (:workbook   left))))

(defn- normalise-range
  "Rewrite whole-col/whole-row halves so both sides are explicit :ref
  nodes. Propagate sheet qualifier from left to right when the right side
  is unqualified (Excel range semantics: a range spans one sheet)."
  [left right]
  (or (when-let [lc (as-whole-col left)]
        (when-let [rc (as-whole-col right)]
          {:op :range :left lc :right (propagate-sheet lc rc)}))
      (when-let [lr (as-whole-row left)]
        (when-let [rr (as-whole-row right)]
          {:op :range :left lr :right (propagate-sheet lr rr)}))
      {:op :range :left left :right (propagate-sheet left right)}))

(def ^:private range-operand-ops
  "AST ops that can stand on either side of the `:` range operator. Numbers
  and names get folded into whole-col/whole-row refs by `normalise-range`
  before this check runs, so after normalisation only :ref / :call / :name
  / :range / :union / :intersect / :table-ref remain legal."
  #{:ref :call :name :range :union :intersect :table-ref})

(defn- range-operand? [node]
  (contains? range-operand-ops (:op node)))

(defn- parse-range
  "Consumes chained `:` after an atom. Range binds tighter than unary and
  postfix, so `-A1:B5` sees range absorb first (→ `-(A1:B5)`). After
  normalisation both sides must be reference-like — `A1:1` and `5:C1`
  raise a parse error (POI: \"RHS of range operator is not a proper
  reference\")."
  [toks i]
  (let [[left i] (parse-atom toks i)]
    (loop [left left, i i]
      (if (= :range (kind toks i))
        (let [colon-i i
              [right j] (parse-atom toks (inc i))
              merged (normalise-range left right)]
          (when-not (range-operand? (:left merged))
            (err! toks colon-i "a reference on the left of ':'"))
          (when-not (range-operand? (:right merged))
            (err! toks colon-i "a reference on the right of ':'"))
          (recur merged j))
        [left i]))))

(def ^:private ref-starting-kinds
  "Token kinds that can start an atomic expression on the right of an
  intersection space. POI treats `1 2` as intersection too, so we accept
  scalar literals as well as refs / calls / groups / table-refs."
  #{:ref :table-ref :ident :num :str :bool :err :lparen :lbrace})

(defn- ref-starting-kind? [k] (contains? ref-starting-kinds k))

(defn- parse-intersection
  "Space between ref expressions is intersection (POI: IntersectionPtg).
  Binds looser than range (`A1:B2 C3:D4` = intersect of two ranges) but
  tighter than postfix — `A1:B2 C3:D4%` is `(intersect)%`, not
  `intersect(A1:B2, C3:D4%)`."
  [toks i]
  (let [[left i] (parse-range toks i)]
    (loop [left left, i i]
      (let [t (peek-tok toks i)]
        (if (and t (:ws-before? t) (ref-starting-kind? (:kind t)))
          (let [[right j] (parse-range toks i)]
            (recur {:op :intersect :left left :right right} j))
          [left i])))))

(defn- parse-postfix
  "Tightens trailing `%` tokens into postop nodes. Multiple percents chain
  (POI: `12345.678%%` parses as two PercentPtg)."
  [toks i]
  (let [[n i] (parse-intersection toks i)]
    (loop [n n, i i]
      (if (and (= :op (kind toks i))
               (= :percent (:value (peek-tok toks i))))
        (recur {:op :postop :sym :percent :arg n} (inc i))
        [n i]))))

(defn- parse-prefix
  "Handles unary +/- and the `@` (implicit-intersection) operator.
  Unary ops bind tighter than `^`, matching Excel (so -3^2 = 9). Unary
  plus is kept as an AST node — this preserves the exact shape of the
  user's formula (POI keeps UnaryPlusPtg for the same reason) so
  serialisation can round-trip."
  [toks i]
  (let [t (peek-tok toks i)]
    (cond
      (and (= :op (:kind t)) (= :minus (:value t)))
      (let [[a i] (parse-prefix toks (inc i))]
        [{:op :unop :sym :minus :arg a} i])
      (and (= :op (:kind t)) (= :plus (:value t)))
      (let [[a i] (parse-prefix toks (inc i))]
        [{:op :unop :sym :plus :arg a} i])
      (= :at (:kind t))
      (let [[a i] (parse-prefix toks (inc i))]
        [{:op :single-cell :arg a} i])
      :else
      (parse-postfix toks i))))

(defn- parse-expr
  "Precedence-climbing loop. `min-prec` is the minimum precedence allowed
  on the right-hand side; tighter operators recurse with higher min-prec."
  [toks i min-prec]
  (let [[left i] (parse-prefix toks i)]
    (loop [left left, i i]
      (if-let [[op p] (tok->binop toks i)]
        (if (>= p min-prec)
          (let [[right i] (parse-expr toks (inc i) (inc p))]
            (recur {:op :binop :sym op :left left :right right} i))
          [left i])
        [left i]))))

(defn- wrap-lex-error
  "Convert a lexer `ex-info` into a structured parse-error shape so
  callers only deal with one error schema."
  [^Throwable e src]
  (let [d   (ex-data e)
        pos (or (:at d) 0)
        [line col] (offset->line-col src pos)
        msg (ex-message e)]
    (ex-info (str "lex error: " msg)
             {:type     :parse-error
              :msg      msg
              :phase    :lex
              :offset   pos
              :line     line
              :col      col
              :source   src
              :ex-data  d}
             e)))

(defn- parse-union-or-expr
  "Top-level entry: parse one expression, then if a comma follows gather
  successive expressions into `{:op :union :args [...]}`. POI allows this
  bare comma-separated form at the top of a formula (`$A:$A,$1:$4`) and
  inside parens — function calls handle commas themselves via parse-args."
  [toks i]
  (let [[e i] (parse-expr toks i 1)]
    (if (= :comma (kind toks i))
      (loop [args [e], i i]
        (if (= :comma (kind toks i))
          (let [[arg j] (parse-expr toks (inc i) 1)]
            (recur (conj args arg) j))
          [{:op :union :args args} i]))
      [e i])))

(defn parse
  "Source string → AST. Throws `ex-info` with structured parse-error data
  (`:type :parse-error`, `:line :col :offset :source`) on failure — pass
  the ex-data to `format-error` for a caret diagnostic."
  [^String src]
  (binding [*src* src]
    (let [toks (try (vec (lex/tokenize src))
                    (catch #?(:clj Throwable :cljs :default) e
                      (if (:type (ex-data e))
                        (throw e)
                        (throw (wrap-lex-error e src)))))
          [ast i] (parse-union-or-expr toks 0)]
      (when (< i (count toks))
        (err! toks i "end of formula"))
      ast)))
