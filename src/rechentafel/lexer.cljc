(ns rechentafel.lexer
  "Tokenizer for Excel formulas. Input: a formula string (without leading '=').
  Output: a vector of tokens, each a map:

    {:kind :num       :value 3.14         :pos 0}
    {:kind :str       :value \"hi\"       :pos 5}
    {:kind :ident     :value \"SUM\"      :pos 10}
    {:kind :ref       :sheet nil
                      :row 0 :col 0
                      :abs-row? false :abs-col? false
                      :text \"A1\"        :pos 14}
    {:kind :ref       :sheet \"Sheet1\"
                      :last-sheet \"Sheet3\"          ;; 3D: Sheet1:Sheet3!A1
                      :workbook \"Book1\"             ;; external: [Book1]...
                      :row 0 :col 0 :pos 14}
    {:kind :table-ref :table \"Table1\"
                      :specifiers [{:kind :column :name \"Col1\"}]
                      :pos 0}
    {:kind :range     :pos 16}                 ;; colon
    {:kind :op        :value :+              :pos 17}
    {:kind :comma     :pos 18}
    {:kind :lparen / :rparen / :lbrace / :rbrace}
    {:kind :err       :value :ref            :pos 20}  ;; #REF!
    {:kind :bool      :value true            :pos 22}

  Design mirrors POI's FormulaParser at the character level (isAlpha/isDigit
  conventions, '$' allowed in identifiers because of absolute refs) but
  produces a token stream so the parser stays declarative."
  (:require [clojure.string :as str]
            [rechentafel.address :as addr]
            [rechentafel.value :as val]))

;; Excel's worksheet bounds (also XFD / 1048576 in A1 notation). A `letters
;; then digits` shape like `Sheet1` would otherwise parse as a cell ref with
;; an absurd column index — we reject those so the lexer can fall through to
;; the ident/sheet-qualifier path.
(def ^:private MAX-COL 16383)      ;; column XFD
(def ^:private MAX-ROW 1048575)    ;; row 1048576

(defn- white? [c] (or (= c \space) (= c \tab) (= c \newline) (= c \return)))
(defn- digit? [c] (and c #?(:clj (Character/isDigit ^char c)
                            :cljs (and (>= (.charCodeAt c 0) 48)
                                       (<= (.charCodeAt c 0) 57)))))
(defn- letter? [c] (and c #?(:clj (Character/isLetter ^char c)
                             :cljs (let [code (.charCodeAt c 0)]
                                     (or (and (>= code 65) (<= code 90))
                                         (and (>= code 97) (<= code 122)))))))
(defn- alpha? [c] (or (letter? c) (= c \_)))
;; `$` is part of an identifier so `$A`, `$1`, `$A$1` scan as one token.
;; Plain `$`-cell-refs like `$A$1` succeed earlier via try-read-ref; the
;; ident path catches `$A`/`$1` (whole-col/row halves), dollar-prefixed
;; sheet names, etc.
(defn- ident-char? [c] (or (alpha? c) (digit? c) (= c \.) (= c \$)))

(defn- take-while-i
  "Walk from index i while (pred (get s j)) holds. Returns [matched new-i]."
  [pred s i]
  (let [n (count s)]
    (loop [j i]
      (if (and (< j n) (pred (get s j)))
        (recur (inc j))
        [(subs s i j) j]))))

(defn- read-number
  "Reads int or decimal with optional exponent. Returns [value new-i]."
  [s i]
  (let [n (count s)]
    (loop [j i, saw-dot? false, saw-e? false]
      (cond
        (>= j n) [(subs s i j) j]
        (digit? (get s j)) (recur (inc j) saw-dot? saw-e?)
        (and (not saw-dot?) (not saw-e?) (= \. (get s j)))
        (recur (inc j) true saw-e?)
        (and (not saw-e?) (or (= \e (get s j)) (= \E (get s j))))
        (let [k (inc j)
              k (if (and (< k n) (or (= \+ (get s k)) (= \- (get s k))))
                  (inc k) k)]
          (recur k saw-dot? true))
        :else [(subs s i j) j]))))

(defn- read-string-literal
  "Excel string literal: double-quoted with \"\"-escape for embedded quote.
  i points at the opening \". Returns [text new-i] excluding the quotes."
  [s i]
  (let [n (count s)]
    (loop [j (inc i), buf ""]
      (cond
        (>= j n) (throw (ex-info "unterminated string" {:at i}))
        (= \" (get s j))
        (if (= \" (get s (inc j) nil))
          (recur (+ j 2) (str buf \"))
          [buf (inc j)])
        :else (recur (inc j) (str buf (get s j)))))))

(defn- read-quoted-sheet
  "Single-quoted sheet name: 'foo bar' with ''-escape. i points at opening '.
  Returns [name new-i] excluding the quotes."
  [s i]
  (let [n (count s)]
    (loop [j (inc i), buf ""]
      (cond
        (>= j n) (throw (ex-info "unterminated sheet name" {:at i}))
        (= \' (get s j))
        (if (= \' (get s (inc j) nil))
          (recur (+ j 2) (str buf \'))
          [buf (inc j)])
        :else (recur (inc j) (str buf (get s j)))))))

(defn- read-error
  "#REF! #DIV/0! #VALUE! #NAME? #NUM! #N/A #NULL! — i points at '#'."
  [s i]
  (let [suffixes [["#REF!"    :ref]
                  ["#DIV/0!"  :div0]
                  ["#VALUE!"  :value]
                  ["#NAME?"   :name]
                  ["#NUM!"    :num]
                  ["#N/A"     :na]
                  ["#NULL!"   :null]
                  ["#GETTING_DATA" :getting-data]]
        up (str/upper-case (subs s i (min (count s) (+ i 20))))]
    (some (fn [[lit code]]
            (when (str/starts-with? up lit)
              [code (+ i (count lit))]))
          suffixes)))

(defn- letters-then-digits
  "Scan letters+$ then digits+$ starting at i. Returns new-i if shape matched
  as a ref (at least one letter then at least one digit), else nil."
  [s i]
  (let [n (count s)]
    (loop [j i, stage :letters, any-letter? false, any-digit? false]
      (if (>= j n)
        (when (and any-letter? any-digit?) j)
        (let [c (get s j)]
          (case stage
            :letters (cond
                       (= c \$) (recur (inc j) :letters any-letter? any-digit?)
                       (letter? c) (recur (inc j) :letters true any-digit?)
                       (and any-letter? (or (= c \$) (digit? c)))
                       (recur j :digits any-letter? any-digit?)
                       :else nil)
            :digits (cond
                      (= c \$) (recur (inc j) :digits any-letter? any-digit?)
                      (digit? c) (recur (inc j) :digits any-letter? true)
                      :else (when (and any-letter? any-digit?) j))))))))

(defn- try-read-ref
  "If s starting at i parses as an A1 cell ref, return [ref-token new-i].
  Consumes $letters$digits. Does not handle sheet prefix — caller does that.
  Backs off when the following char would extend the token into a longer
  identifier: `!` (sheet qualifier), `_`, or another letter. Without this
  backoff `DA6_LEO_WBS` would tokenise as ref `DA6` then name `_LEO_WBS`.
  Also rejects out-of-range cells (col > XFD, row > 1048576) so `Sheet1`
  falls through to the ident path rather than scanning as a huge column."
  [s i]
  (when-let [end (letters-then-digits s i)]
    (let [next-c (get s end nil)]
      (when-not (or (= \! next-c)
                    (= \_ next-c)
                    (= \( next-c)          ;; LOG10(...) — function call, not a ref
                    (and next-c (letter? next-c)))
        (let [text (subs s i end)]
          (when-let [parsed (addr/parse-a1 text)]
            (when (and (<= (:col parsed) MAX-COL)
                       (<= (:row parsed) MAX-ROW))
              [(assoc parsed :kind :ref :text text :pos i) end])))))))

(defn- try-read-col-or-row-ref
  "Whole-column (A:A) or whole-row (1:1) refs. Returns nil if not matched.
  Matches *just the left side*: the parser will pair with `:range` + right.
  Dispatches on what follows an optional `$` so that `$A` → whole-col and
  `$1` → whole-row without the two branches stealing each other's work."
  [s i]
  (let [n   (count s)
        c   (get s i nil)
        c2  (get s (inc i) nil)
        col? (or (and c (letter? c))
                 (and (= \$ c) c2 (letter? c2)))
        row? (or (and c (digit? c))
                 (and (= \$ c) c2 (digit? c2)))]
    (cond
      (and (< i n) col?)
      (let [[letters j] (take-while-i #(or (letter? %) (= \$ %)) s i)]
        (when (seq (str/replace letters #"\$" ""))
          [{:kind :ref :text letters
            :col (addr/col-letters->idx (str/replace letters #"\$" ""))
            :abs-col? (str/starts-with? letters "$")
            :pos i
            :whole :col}
           j]))

      (and (< i n) row?)
      (let [[digits j] (take-while-i #(or (digit? %) (= \$ %)) s i)
            stripped (str/replace digits #"\$" "")]
        (when (seq stripped)
          [{:kind :ref :text digits
            :row (dec #?(:clj  (Long/parseLong stripped)
                         :cljs (js/parseInt stripped 10)))
            :abs-row? (str/starts-with? digits "$")
            :pos i
            :whole :row}
           j])))))

;; ---------------------------------------------------------------------------
;; Main dispatch

(defn- one-op
  "Try to consume a 1- or 2-char operator. Returns [op-kw new-i] or nil."
  [s i]
  (let [c (get s i)
        c2 (get s (inc i))]
    (case c
      \< (cond (= c2 \=) [:le (+ i 2)]
               (= c2 \>) [:ne (+ i 2)]
               :else     [:lt (inc i)])
      \> (if (= c2 \=) [:ge (+ i 2)] [:gt (inc i)])
      \= [:eq      (inc i)]
      \+ [:plus    (inc i)]
      \- [:minus   (inc i)]
      \* [:mul     (inc i)]
      \/ [:div     (inc i)]
      \^ [:pow     (inc i)]
      \& [:concat  (inc i)]
      \% [:percent (inc i)]
      nil)))

(defn- read-sheet-qualified
  "After we've consumed a sheet name and an `!`, read what follows as either
  a cell ref (`Sheet1!A1`) or a whole-col / whole-row half (`Sheet1!A:A`,
  `Sheet1!$A:$A`, `Sheet1!1:2`). Returns [ref-token new-i] or nil."
  [s i sheet]
  (or (when-let [[ref j] (try-read-ref s i)]
        [(assoc ref :sheet sheet) j])
      (when-let [[ref j] (try-read-col-or-row-ref s i)]
        [(assoc ref :sheet sheet) j])))

(defn- read-sheet-identifier
  "Read a sheet name — quoted or unquoted — and return [name new-i], or nil
  when the position doesn't start a sheet name. Unquoted sheets reuse the
  ident-char class so `$`-prefixed names work too."
  [s i]
  (cond
    (= \' (get s i nil))
    (read-quoted-sheet s i)

    (or (alpha? (get s i nil))
        (= \$ (get s i nil)))
    (let [[ident j] (take-while-i ident-char? s i)]
      (when (pos? (count ident))
        [ident j]))

    :else nil))

(defn- try-read-workbook-bracket
  "Optional `[bookname]` prefix. Returns [name new-i] or nil. `bookname` is
  every character up to the matching `]`. Nested brackets are rejected —
  POI doesn't allow them in workbook specs."
  [s i]
  (when (= \[ (get s i nil))
    (let [n (count s)]
      (loop [j (inc i), buf ""]
        (cond
          (>= j n) nil
          (= \] (get s j)) [buf (inc j)]
          (= \[ (get s j)) nil
          :else (recur (inc j) (str buf (get s j))))))))

(defn- read-prefixed-ref
  "Reads the sheet-/workbook-prefixed ref grammars in one pass:

      Sheet!ref                    — 2D
      Sheet1:Sheet2!ref            — 3D (sheet range)
      'Sheet'!ref                  — quoted
      'Sheet1':'Sheet2'!ref        — 3D quoted
      [Book]Sheet!ref              — 2D external
      [Book]Sheet1:Sheet2!ref      — 3D external
      [Book]!ref                   — workbook without sheet

  Returns [ref-token new-i] or nil if the input doesn't start with one of
  these shapes. Does *not* consume trailing whitespace."
  [s i0]
  (let [[book i1] (or (try-read-workbook-bracket s i0) [nil i0])
        [sheet1 j] (or (read-sheet-identifier s i1) [nil i1])]
    (cond
      (and book (nil? sheet1) (= \! (get s i1 nil)))
      (when-let [[ref k] (read-sheet-qualified s (inc i1) nil)]
        [(assoc ref :workbook book :pos i0) k])

      (and sheet1 (= \: (get s j nil)))
      (when-let [[sheet2 k] (read-sheet-identifier s (inc j))]
        (when (= \! (get s k nil))
          (when-let [[ref m] (read-sheet-qualified s (inc k) sheet1)]
            [(cond-> (assoc ref :last-sheet sheet2 :pos i0)
               book (assoc :workbook book))
             m])))

      (and sheet1 (= \! (get s j nil)))
      (when-let [[ref k] (read-sheet-qualified s (inc j) sheet1)]
        [(cond-> (assoc ref :pos i0)
           book (assoc :workbook book))
         k])

      :else nil)))

;; ---------------------------------------------------------------------------
;; Structured (table) references — POI's StructuredReference grammar.
;;
;;   Table1[Col1]
;;   Table1[[Col1]:[Col2]]
;;   Table1[#Headers]   #Data   #Totals   #All
;;   Table1[@Col1]                                (shortcut for #This Row)
;;   Table1[[#Headers],[Col1]]
;;   Table1[[#This Row],[Col1]:[Col2]]
;;   [Col1]                                       (implicit-table, used in
;;                                                 a formula that lives
;;                                                 inside the table)
;;
;; We emit a single {:kind :table-ref :table <name-or-nil> :specifiers [...]}
;; token with the inside already parsed into items; the outer parser treats
;; it as an atomic reference.

(def ^:private area-keyword
  {"HEADERS"  :headers
   "DATA"     :data
   "TOTALS"   :totals
   "ALL"      :all
   "THIS ROW" :this-row})

(defn- parse-area-keyword [s]
  (area-keyword (str/upper-case (str/trim s))))

(defn- read-balanced-brackets
  "Assumes s[i] = '['. Reads until the matching ']', returning
  [inner-content new-i]. Tracks nesting depth — POI allows one level of
  inner `[...]` inside the outer structured-ref bracket."
  [s i]
  (let [n (count s)]
    (loop [j (inc i), depth 1, buf ""]
      (cond
        (>= j n)
        (throw (ex-info "unterminated '[' in structured reference" {:at i}))
        (= \[ (get s j))
        (recur (inc j) (inc depth) (str buf (get s j)))
        (= \] (get s j))
        (if (= 1 depth)
          [buf (inc j)]
          (recur (inc j) (dec depth) (str buf (get s j))))
        :else (recur (inc j) depth (str buf (get s j)))))))

(defn- split-top-level-commas
  "Split on commas that are *not* inside a `[...]` group."
  [s]
  (let [n (count s)]
    (loop [i 0, start 0, depth 0, out []]
      (cond
        (>= i n) (conj out (subs s start i))
        (= \[ (get s i)) (recur (inc i) start (inc depth) out)
        (= \] (get s i)) (recur (inc i) start (dec depth) out)
        (and (zero? depth) (= \, (get s i)))
        (recur (inc i) (inc i) depth (conj out (subs s start i)))
        :else (recur (inc i) start depth out)))))

(def ^:private column-range-re
  #"^\[([^\]]+)\]\s*:\s*\[([^\]]+)\]$")
(def ^:private bracket-area-re  #"^\[\s*#\s*(.+?)\s*\]$")
(def ^:private bracket-col-re   #"^\[([^\]]+)\]$")
(def ^:private bare-area-re     #"^#\s*(.+)$")

(defn- parse-single-spec-item [chunk pos]
  (let [c (str/trim chunk)]
    (cond
      (empty? c)
      (throw (ex-info "empty structured-ref item" {:at pos}))

      (re-matches column-range-re c)
      (let [[_ from to] (re-matches column-range-re c)]
        {:kind :column-range :from from :to to})

      (re-matches bracket-area-re c)
      (let [[_ kw] (re-matches bracket-area-re c)
            code   (parse-area-keyword kw)]
        (when (nil? code)
          (throw (ex-info "unknown # specifier" {:text c :at pos})))
        {:kind :area :value code})

      (re-matches bracket-col-re c)
      (let [[_ name] (re-matches bracket-col-re c)]
        {:kind :column :name name})

      (re-matches bare-area-re c)
      (let [[_ kw] (re-matches bare-area-re c)
            code   (parse-area-keyword kw)]
        (when (nil? code)
          (throw (ex-info "unknown # specifier" {:text c :at pos})))
        {:kind :area :value code})

      :else
      {:kind :column :name c})))

(defn- parse-table-spec
  "Parse the content between the outer `[` and `]` of a structured ref into
  a vector of specifier items. A leading `@` expands to a `:this-row` area
  item (matching POI's `[@...]` shorthand)."
  [content pos]
  (let [c (str/trim content)]
    (cond
      (empty? c) []

      (str/starts-with? c "@")
      (into [{:kind :area :value :this-row}]
            (parse-table-spec (subs c 1) (inc pos)))

      :else
      (mapv #(parse-single-spec-item % pos)
            (split-top-level-commas c)))))

(defn- read-structured-ref
  "Read `Table1[spec]` or `[spec]` (implicit-table). `i` points at `[`.
  Returns [table-ref-token new-i]. `table-name` is nil for implicit refs."
  [s i table-name token-pos]
  (let [[content j] (read-balanced-brackets s i)
        specs       (parse-table-spec content i)]
    [{:kind       :table-ref
      :table      table-name
      :specifiers specs
      :pos        token-pos}
     j]))

(defn- try-read-implicit-structured-ref
  "`[spec]` at a ref position, not preceded by an ident. We distinguish from
  `[Book]Sheet!ref` by peeking past the matching `]`: if what follows is a
  sheet-name starter (`!`, letter, `'`), the bracket is a workbook spec and
  this function returns nil so `read-prefixed-ref` can handle it."
  [s i]
  (when (= \[ (get s i nil))
    (when-let [[_ after] (try-read-workbook-bracket s i)]
      (let [c (get s after nil)]
        (when-not (or (= \! c) (= \' c) (and c (alpha? c)))
          (read-structured-ref s i nil i))))))

(defn tokenize
  "Turn a formula string into a vector of tokens.

  Each token carries `:pos` (char offset) and `:ws-before?` (true when
  preceded by whitespace — needed by the parser to recognise the
  intersection operator, which is a bare space between ref expressions)."
  [^String src]
  (let [s (if (and (pos? (count src)) (= \= (get src 0)))
            (subs src 1)
            src)
        n (count s)]
    (loop [i 0, ws? false, toks (transient [])]
      (cond
        (>= i n) (persistent! toks)

        (white? (get s i))
        (recur (inc i) true toks)

        :else
        (let [[tok j]
              (cond
                (= \" (get s i))
                (let [[text j] (read-string-literal s i)]
                  [{:kind :str :value text :pos i} j])

                (= \# (get s i))
                (if-let [[code j] (read-error s i)]
                  [{:kind :err :value code :pos i} j]
                  ;; Bare `#` — Excel 365 spill-range operator (postfix
                  ;; after a ref: `A1#`). Stored as `_xlfn.ANCHORARRAY`
                  ;; in OOXML. The parser turns ref-then-:hash into a
                  ;; `:spill-ref` AST node.
                  [{:kind :hash :pos i} (inc i)])

                (or (digit? (get s i))
                    (and (= \. (get s i)) (digit? (get s (inc i)))))
                (let [[raw j] (read-number s i)]
                  [{:kind :num
                    :value #?(:clj  (Double/parseDouble raw)
                              :cljs (js/parseFloat raw))
                    :pos i} j])

                (= \( (get s i)) [{:kind :lparen :pos i} (inc i)]
                (= \) (get s i)) [{:kind :rparen :pos i} (inc i)]
                (= \{ (get s i)) [{:kind :lbrace :pos i} (inc i)]
                (= \} (get s i)) [{:kind :rbrace :pos i} (inc i)]
                (= \, (get s i)) [{:kind :comma  :pos i} (inc i)]
                (= \; (get s i)) [{:kind :semi   :pos i} (inc i)]
                (= \: (get s i)) [{:kind :range  :pos i} (inc i)]
                ;; `@` — implicit-intersection / single-cell operator
                ;; (Excel 365). Stored as `_xlfn.SINGLE(...)` in OOXML.
                ;; Inside `[…]` brackets `@` is the table-ref this-row
                ;; shorthand and is handled by the structured-ref reader,
                ;; so this lexer path only fires at expression scope.
                (= \@ (get s i)) [{:kind :at :pos i} (inc i)]

                :else
                (or
                 ;; Sheet- / workbook-prefixed ref. Tried before bare
                 ;; cell-refs so that `S1:S3!A1` (3D) doesn't get split
                 ;; as `cell-ref(S1) range S3!A1`. The prefixed grammar
                 ;; requires a `!`, so it falls through to bare-cell on
                 ;; input like `S1+1`.
                 (read-prefixed-ref s i)

                 ;; Bare cell ref (no sheet prefix, in-range).
                 (try-read-ref s i)

                 ;; Identifier — possibly followed by `[...]` to form a
                 ;; structured table reference (`Table1[Col]`). Handles
                 ;; TRUE/FALSE as well.
                 (when (or (alpha? (get s i))
                           (= \$ (get s i)))
                   (let [[ident j] (take-while-i ident-char? s i)]
                     (cond
                       (= \[ (get s j nil))
                       (read-structured-ref s j ident i)
                       (= "TRUE"  (str/upper-case ident))
                       [{:kind :bool :value true  :pos i} j]
                       (= "FALSE" (str/upper-case ident))
                       [{:kind :bool :value false :pos i} j]
                       :else
                       [{:kind :ident :value ident :pos i} j])))

                 ;; Implicit-table structured ref (`[Col]` with no table
                 ;; name). Must be tried *after* read-prefixed-ref so that
                 ;; `[Book]Sheet!A1` doesn't get stolen.
                 (try-read-implicit-structured-ref s i)

                 ;; Whole-column / whole-row ref (A:A, 1:1)
                 (try-read-col-or-row-ref s i)

                 ;; Operator
                 (when-let [[op j] (one-op s i)]
                   [{:kind :op :value op :pos i} j])

                 (throw (ex-info "unexpected char"
                                 {:at i :char (get s i)}))))]
          (when-not (< i j)
            (throw (ex-info "lexer did not advance"
                            {:at i :tok tok})))
          (recur j false (conj! toks (assoc tok :ws-before? ws?))))))))
