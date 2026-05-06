(ns rechentafel.unparse
  "AST → formula source string. Inverse of `rechentafel.parser/parse`.

  Produces *canonical* output: function names uppercase, operators
  unspaced, integers without trailing `.0`, sheet names quoted only
  when required. Round-trips at the AST level (parse(unparse(ast)) ≡
  ast), not at the source-string level (unparse(parse(s)) may differ
  from s in whitespace/case)."
  (:require [clojure.string :as str]
            [rechentafel.address :as addr]))

;; ---------------------------------------------------------------------------
;; Sheet-name quoting

(def ^:private safe-sheet-re
  "Sheet names made of [A-Za-z_][A-Za-z0-9_.]* don't need quoting."
  #"^[A-Za-z_][A-Za-z0-9_.]*$")

(defn- quote-sheet [^String s]
  (if (and s (re-matches safe-sheet-re s))
    s
    (str \' (str/replace (or s "") "'" "''") \')))

(defn- sheet-prefix
  "Render the sheet (and optional last-sheet for 3D, workbook for
  external) prefix of a ref/range, ending with `!`. Returns \"\" if no
  qualifier."
  [{:keys [sheet last-sheet workbook]}]
  (cond
    (and (nil? sheet) (nil? workbook)) ""
    :else
    (let [book   (when workbook (str \[ workbook \]))
          first  (when sheet (quote-sheet sheet))
          last   (when last-sheet (str ":" (quote-sheet last-sheet)))]
      (str book first last "!"))))

;; ---------------------------------------------------------------------------
;; Ref formatting

(defn- ref-only [{:keys [row col abs-row? abs-col? whole] :as r}]
  (case whole
    :col (str (when abs-col? "$") (addr/col-idx->letters (long col)))
    :row (str (when abs-row? "$") (inc (long row)))
    (addr/format-a1 (long row) (long col) (boolean abs-row?) (boolean abs-col?))))

(defn- format-ref [r]
  (str (sheet-prefix r) (ref-only r)))

;; ---------------------------------------------------------------------------
;; Numbers / strings / errors

(defn- finite? [^double d]
  #?(:clj  (Double/isFinite d)
     :cljs (js/isFinite d)))

(defn- format-number [n]
  ;; Canonical: integers without trailing .0, doubles via str.
  (let [d (double n)]
    (if (and (finite? d)
             (== d (Math/floor d))
             (<= (Math/abs d) 1e15))
      (str (long d))
      (str d))))

(defn- format-string [s]
  (str \" (str/replace (or s "") "\"" "\"\"") \"))

(def ^:private err-text
  {:null  "#NULL!"
   :div0  "#DIV/0!"
   :value "#VALUE!"
   :ref   "#REF!"
   :name  "#NAME?"
   :num   "#NUM!"
   :na    "#N/A"
   :getting-data "#GETTING_DATA"})

;; ---------------------------------------------------------------------------
;; Binop / unop / postop precedence and parenthesisation

(def ^:private binop-prec
  {:eq 1 :ne 1 :lt 1 :le 1 :gt 1 :ge 1
   :concat 2
   :plus 3 :minus 3
   :mul 4 :div 4
   :pow 5})

(def ^:private binop-text
  {:eq "=" :ne "<>" :lt "<" :le "<=" :gt ">" :ge ">="
   :concat "&"
   :plus "+" :minus "-" :mul "*" :div "/" :pow "^"})

(def ^:private right-assoc? #{:pow})

(defn- needs-parens?
  "Whether `child` needs parens when it sits on `side` (:left or :right)
  of a binop with precedence `parent-prec`."
  [child parent-prec side parent-sym]
  (case (:op child)
    :binop
    (let [cp (binop-prec (:sym child))]
      (cond
        (< cp parent-prec) true
        (> cp parent-prec) false
        ;; equal precedence: left-assoc unless the parent is right-assoc
        (= side :right) (not (right-assoc? parent-sym))
        :else false))
    :union  true       ;; (a,b) is a union, always parenthesise inline
    false))

(declare unparse)

(defn- unparse-binop [{:keys [sym left right]}]
  (let [p  (binop-prec sym)
        l  (unparse left)
        r  (unparse right)
        l  (if (needs-parens? left  p :left  sym) (str "(" l ")") l)
        r  (if (needs-parens? right p :right sym) (str "(" r ")") r)]
    (str l (binop-text sym) r)))

(defn- unparse-unop [{:keys [sym arg]}]
  (let [a   (unparse arg)
        ;; unary applies before pow (-3^2 = 9 in Excel, so -(3^2) — already
        ;; structured that way by the parser). Wrap binops conservatively
        ;; for readability.
        a   (if (= :binop (:op arg)) (str "(" a ")") a)]
    (str (case sym :minus "-" :plus "+") a)))

(defn- unparse-postop [{:keys [sym arg]}]
  (let [a (unparse arg)
        a (if (= :binop (:op arg)) (str "(" a ")") a)]
    (str a (case sym :percent "%"))))

(defn- unparse-single-cell [{:keys [arg]}]
  (let [a (unparse arg)
        ;; `:range` is fine bare (`@A1:A10`); other compound ops need
        ;; parens so `@` binds tightly.
        a (if (#{:binop :unop :postop :intersect :union} (:op arg))
            (str "(" a ")") a)]
    (str "@" a)))

(defn- unparse-spill-ref [{:keys [anchor]}]
  (str (unparse anchor) "#"))

;; ---------------------------------------------------------------------------
;; Range / union / intersection

(defn- unparse-range [{:keys [left right]}]
  ;; The parser propagates the sheet qualifier from left to right; for
  ;; canonical output we render the qualifier once on the left and bare
  ;; ref on the right (Excel's preferred form).
  (let [l   (unparse left)
        r   (-> right
                (cond-> (= :ref (:op right))
                  (-> (dissoc :sheet :last-sheet :workbook)))
                unparse)]
    (str l ":" r)))

(defn- unparse-union [{:keys [args]}]
  (str "(" (str/join "," (map unparse args)) ")"))

(defn- unparse-intersect [{:keys [left right]}]
  (str (unparse left) " " (unparse right)))

;; ---------------------------------------------------------------------------
;; Calls + arrays + table refs

(defn- unparse-call [{:keys [name args]}]
  (str (str/upper-case name)
       "("
       (str/join "," (map unparse args))
       ")"))

(defn- unparse-array [{:keys [rows]}]
  (str "{"
       (str/join ";"
                 (for [row rows]
                   (str/join "," (map unparse row))))
       "}"))

(defn- unparse-spec-item [{:keys [kind name from to value]}]
  (case kind
    :column      (str "[" name "]")
    :column-range (str "[" from "]:[" to "]")
    :area        (case value
                   :all      "[#All]"
                   :headers  "[#Headers]"
                   :data     "[#Data]"
                   :totals   "[#Totals]"
                   :this-row "[#This Row]")))

(defn- unparse-let [{:keys [bindings body]}]
  (str "LET("
       (str/join "," (concat (mapcat (fn [[n v]] [n (unparse v)]) bindings)
                             [(unparse body)]))
       ")"))

(defn- unparse-lambda [{:keys [params body]}]
  (str "LAMBDA("
       (str/join "," (concat
                      (map (fn [p]
                             (if (:optional? p)
                               (str "[" (:name p) "]")
                               (:name p)))
                           params)
                      [(unparse body)]))
       ")"))

(defn- unparse-lambda-call [{:keys [fn args]}]
  (let [callee (unparse fn)
        ;; LAMBDA(...)(args) — wrap in parens only when the callee is
        ;; a complex expression that wouldn't otherwise group correctly.
        ;; Bare :lambda / :name / :call render unambiguously; binops do
        ;; not.
        callee (if (#{:binop :unop :postop :range :intersect :union} (:op fn))
                 (str "(" callee ")")
                 callee)]
    (str callee "(" (str/join "," (map unparse args)) ")")))

(defn- unparse-table-ref [{:keys [table specifiers]}]
  (let [items (map unparse-spec-item specifiers)
        inner (cond
                (empty? items)        ""
                (= 1 (count items))   (first items)
                :else                 (str "[" (str/join "," items) "]"))]
    (str (or table "") inner)))

;; ---------------------------------------------------------------------------
;; Public

(defn unparse
  "AST node → canonical formula source string. The result does NOT
  include a leading `=`."
  [ast]
  (case (:op ast)
    :num     (format-number (:value ast))
    :str     (format-string (:value ast))
    :bool    (if (:value ast) "TRUE" "FALSE")
    :err     (or (err-text (:value ast)) (str "#" (str/upper-case (clojure.core/name (:value ast))) "!"))
    :missing ""
    :ref     (format-ref ast)
    :name    (:value ast)
    :binop   (unparse-binop ast)
    :unop    (unparse-unop ast)
    :postop  (unparse-postop ast)
    :single-cell (unparse-single-cell ast)
    :spill-ref (unparse-spill-ref ast)
    :range   (unparse-range ast)
    :union   (unparse-union ast)
    :intersect (unparse-intersect ast)
    :call    (unparse-call ast)
    :array   (unparse-array ast)
    :table-ref (unparse-table-ref ast)
    :let     (unparse-let ast)
    :lambda  (unparse-lambda ast)
    :lambda-call (unparse-lambda-call ast)))
