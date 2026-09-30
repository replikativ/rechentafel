(ns rechentafel.fn.xml
  "FILTERXML(xml, xpath): the nodes an XPath selects in an XML text, as
   Excel returns them — one node a value, several a column, numeric text a
   number, none #VALUE!. The common use splits text:
   FILTERXML(\"<t><s>\"&SUBSTITUTE(A1,\",\",\"</s><s>\")&\"</s></t>\",\"//s\").

   The XML is read strictly (well-formed or #VALUE!) and without a DTD, so
   no entity is expanded beyond the five predefined and character
   references. The XPath is XPath 1.0 without namespaces and variables:
   location paths over every axis (child, descendant, parent, ancestor,
   following/preceding(-sibling), attribute, self and their -or-self),
   name, *, text() and node() tests, predicates, the operators (or and = !=
   < <= > >= + - * div mod |) with XPath's comparison of node-sets, and the
   core function library."
  (:require [clojure.string :as str]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; XML → a document: a vector of nodes in document order, the root at 0.
;; A node is {:kind :root|:elem|:attr|:text :name :value :parent :children
;; :attrs :end}; :end is the id of its last descendant.

(defn- malformed! [] (throw (ex-info "malformed" {::malformed true})))

(defn- parse-int [s radix]
  #?(:clj (Long/parseLong s (int radix)) :cljs (js/parseInt s radix)))

(defn- code-point->str [cp]
  #?(:clj (String. (Character/toChars (int cp))) :cljs (.fromCodePoint js/String cp)))

(defn- decode
  "`s` with its entity and character references replaced; a bare & is
   malformed."
  [s]
  (if-not (str/includes? s "&")
    s
    (do (when (re-find #"&(?!(?:#x[0-9a-fA-F]+|#[0-9]+|amp|lt|gt|quot|apos);)" s) (malformed!))
        (str/replace s #"&(#x[0-9a-fA-F]+|#[0-9]+|amp|lt|gt|quot|apos);"
                     (fn [[_ e]]
                       (case e
                         "amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'"
                         (code-point->str (if (str/starts-with? e "#x")
                                            (parse-int (subs e 2) 16)
                                            (parse-int (subs e 1) 10)))))))))

(defn- ws? [c] (and c (#{\space \tab \newline \return} c)))

(defn- name-end
  "The index after the XML name at `i` in `s` (i itself when there is none)."
  [s i]
  (let [n (count s)]
    (loop [j i]
      (if (and (< j n) (not (ws? (nth s j))) (not (#{\/ \> \= \< \" \' \! \?} (nth s j))))
        (recur (inc j))
        j))))

(defn- skip-ws [s i]
  (let [n (count s)] (loop [j i] (if (and (< j n) (ws? (nth s j))) (recur (inc j)) j))))

(defn- parse-xml
  "`s` as a document, or nil when it is not well-formed XML."
  [s]
  (let [n (count s)
        nodes (volatile! [{:kind :root :parent nil :children [] :attrs []}])
        add! (fn [node]
               (let [id (count @nodes)]
                 (vswap! nodes conj (assoc node :id id :end id))
                 (vswap! nodes update-in [(:parent node) (if (= :attr (:kind node)) :attrs :children)] conj id)
                 id))
        close! (fn [id] (vswap! nodes assoc-in [id :end] (dec (count @nodes))))
        starts? (fn [i prefix] (= prefix (subs s i (min n (+ i (count prefix))))))
        skip-to (fn [i end] (if-let [j (str/index-of s end i)] (+ j (count end)) (malformed!)))]
    (try
      (loop [i 0 stack [0] root? false]
        (if (>= i n)
          (if (and root? (= 1 (count stack)))
            (let [doc @nodes] (assoc-in doc [0 :end] (dec (count doc))))
            (malformed!))
          (if (= \< (nth s i))
            (cond
              (starts? i "<!--") (recur (skip-to (+ i 4) "-->") stack root?)
              (starts? i "<?") (recur (skip-to (+ i 2) "?>") stack root?)
              (starts? i "<![CDATA[")
              (let [j (or (str/index-of s "]]>" (+ i 9)) (malformed!))]
                (when (= 1 (count stack)) (malformed!))
                (add! {:kind :text :value (subs s (+ i 9) j) :parent (peek stack)})
                (recur (+ j 3) stack root?))
              ;; a DTD (<!DOCTYPE) is refused: nothing to expand
              (starts? i "<!") (malformed!)
              (starts? i "</")
              (let [j (name-end s (+ i 2))
                    nm (subs s (+ i 2) j)
                    k (skip-ws s j)
                    top (peek stack)]
                (when (or (= 1 (count stack)) (not= nm (:name (nth @nodes top))) (not= \> (get s k))) (malformed!))
                (close! top)
                (recur (inc k) (pop stack) root?))
              :else
              (let [j (name-end s (inc i))
                    nm (subs s (inc i) j)
                    _ (when (or (str/blank? nm) (and root? (= 1 (count stack)))) (malformed!))
                    id (add! {:kind :elem :name nm :parent (peek stack) :children [] :attrs []})
                    [k self?] (loop [k j]
                                (let [k (skip-ws s k)]
                                  (cond
                                    (starts? k "/>") [(+ k 2) true]
                                    (starts? k ">") [(inc k) false]
                                    :else
                                    (let [e (name-end s k)
                                          an (subs s k e)
                                          q (skip-ws s (skip-ws s e))
                                          _ (when (or (str/blank? an) (not= \= (get s q))) (malformed!))
                                          v0 (skip-ws s (inc q))
                                          qc (get s v0)
                                          _ (when-not (#{\" \'} qc) (malformed!))
                                          v1 (or (str/index-of s (str qc) (inc v0)) (malformed!))
                                          raw (subs s (inc v0) v1)]
                                      (when (str/includes? raw "<") (malformed!))
                                      (add! {:kind :attr :name an :value (decode raw) :parent id})
                                      (recur (inc v1))))))]
                (if self?
                  (do (close! id) (recur k stack true))
                  (recur k (conj stack id) true))))
            (let [j (or (str/index-of s "<" i) n)
                  t (subs s i j)]
              (if (= 1 (count stack))
                (if (str/blank? t) (recur j stack root?) (malformed!))
                (do (add! {:kind :text :value (decode t) :parent (peek stack)})
                    (recur j stack root?)))))))
      (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
        (if (::malformed (ex-data e)) nil (throw e))))))

;; ---------------------------------------------------------------------------
;; XPath: tokens → an expression tree

(def ^:private operator-names #{"and" "or" "div" "mod"})

(defn- operand-end?
  "Whether token `t` ends an operand, so a following * or name is an
   operator (XPath 1.0 §3.7)."
  [t]
  (and t (or (#{:name :num :lit :dot :dotdot :star} (first t))
             (#{[:op ")"] [:op "]"]} t))))

(defn- tokenize [s]
  (let [n (count s)]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [c (nth s i)
              two (subs s i (min n (+ i 2)))
              prev (peek out)]
          (cond
            (ws? c) (recur (inc i) out)
            (= two "//") (recur (+ i 2) (conj out [:op "//"]))
            (= two "::") (recur (+ i 2) (conj out [:op "::"]))
            (= two "..") (recur (+ i 2) (conj out [:dotdot]))
            (#{"!=" "<=" ">="} two) (recur (+ i 2) (conj out [:op two]))
            (or (#?(:clj Character/isDigit :cljs #(re-matches #"\d" (str %))) c)
                (and (= c \.) (< (inc i) n) (re-matches #"\d" (str (nth s (inc i))))))
            (let [m (re-find #"^\d*\.?\d*" (subs s i))]
              (recur (+ i (count m)) (conj out [:num #?(:clj (Double/parseDouble m) :cljs (js/parseFloat m))])))
            (= c \.) (recur (inc i) (conj out [:dot]))
            (= c \*) (recur (inc i) (conj out (if (operand-end? prev) [:mul] [:star])))
            (#{\/ \[ \] \( \) \@ \, \| \= \< \> \+ \-} c) (recur (inc i) (conj out [:op (str c)]))
            (#{\" \'} c) (let [j (or (str/index-of s (str c) (inc i)) (malformed!))]
                           (recur (inc j) (conj out [:lit (subs s (inc i) j)])))
            :else
            (let [m (re-find #"^[A-Za-z_À-￿][-A-Za-z0-9_.·À-￿]*(?::[A-Za-z_À-￿][-A-Za-z0-9_.À-￿]*)?" (subs s i))]
              (when-not m (malformed!))
              (recur (+ i (count m))
                     (conj out (if (and (operand-end? prev) (operator-names m)) [:kw m] [:name m]))))))))))

(def ^:private axes
  #{"ancestor" "ancestor-or-self" "attribute" "child" "descendant" "descendant-or-self"
    "following" "following-sibling" "parent" "preceding" "preceding-sibling" "self"})

(def ^:private descendant-or-self-step {:axis :descendant-or-self :test [:node] :preds []})

(defn- parse-xpath [s]
  (let [tokens (tokenize s)
        pos (volatile! 0)
        peek* (fn ([] (get tokens @pos)) ([k] (get tokens (+ @pos k))))
        next! (fn [] (let [t (get tokens @pos)] (vswap! pos inc) t))
        expect! (fn [t] (when-not (= t (next!)) (malformed!)))]
    (letfn [(binary [sub ops ctor]
              (loop [a (sub)]
                (if-let [op (ops (peek*))]
                  (do (next!) (recur (ctor op a (sub))))
                  a)))
            (or-expr [] (binary and-expr #{[:kw "or"]} (fn [_ a b] [:or a b])))
            (and-expr [] (binary eq-expr #{[:kw "and"]} (fn [_ a b] [:and a b])))
            (eq-expr [] (binary rel-expr #{[:op "="] [:op "!="]} (fn [op a b] [:cmp (second op) a b])))
            (rel-expr [] (binary add-expr #{[:op "<"] [:op "<="] [:op ">"] [:op ">="]} (fn [op a b] [:cmp (second op) a b])))
            (add-expr [] (binary mul-expr #{[:op "+"] [:op "-"]} (fn [op a b] [:arith (second op) a b])))
            (mul-expr [] (binary unary #{[:mul] [:kw "div"] [:kw "mod"]}
                                 (fn [op a b] [:arith (if (= [:mul] op) "*" (second op)) a b])))
            (unary [] (if (= [:op "-"] (peek*)) (do (next!) [:neg (unary)]) (union-expr)))
            (union-expr [] (binary path-expr #{[:op "|"]} (fn [_ a b] [:union a b])))
            (step-start? [t] (or (#{[:dot] [:dotdot] [:op "@"] [:star]} t) (= :name (first t))))
            (primary-start? []
              (let [t (peek*)]
                (or (#{:num :lit} (first t)) (= [:op "("] t)
                    (and (= :name (first t)) (= [:op "("] (peek* 1))
                         (not (#{"text" "node" "comment" "processing-instruction"} (second t)))))))
            (path-expr []
              (let [t (peek*)]
                (cond
                  (= [:op "/"] t) (do (next!) [:path true (if (step-start? (peek*)) (relative) [])])
                  (= [:op "//"] t) (do (next!) [:path true (into [descendant-or-self-step] (relative))])
                  (primary-start?) (let [p (primary) preds (predicates) steps (more-steps [])]
                                     (if (and (empty? preds) (empty? steps)) p [:filter p preds steps]))
                  :else [:path false (relative)])))
            (primary []
              (let [t (next!)]
                (case (first t)
                  :num [:num (second t)]
                  :lit [:str (second t)]
                  :op (let [e (or-expr)] (expect! [:op ")"]) e)
                  :name (do (expect! [:op "("])
                            (let [args (if (= [:op ")"] (peek*))
                                         []
                                         (loop [as [(or-expr)]]
                                           (if (= [:op ","] (peek*)) (do (next!) (recur (conj as (or-expr)))) as)))]
                              (expect! [:op ")"])
                              [:call (second t) args])))))
            (more-steps [steps]
              (cond
                (= [:op "/"] (peek*)) (do (next!) (more-steps (conj steps (step))))
                (= [:op "//"] (peek*)) (do (next!) (more-steps (conj steps descendant-or-self-step (step))))
                :else steps))
            (relative [] (more-steps [(step)]))
            (step []
              (let [t (peek*)]
                (cond
                  (= [:dot] t) (do (next!) {:axis :self :test [:node] :preds (predicates)})
                  (= [:dotdot] t) (do (next!) {:axis :parent :test [:node] :preds (predicates)})
                  :else
                  (let [axis (cond
                               (= [:op "@"] t) (do (next!) :attribute)
                               (and (= :name (first t)) (= [:op "::"] (peek* 1)))
                               (do (when-not (axes (second t)) (malformed!)) (next!) (next!) (keyword (second t)))
                               :else :child)
                        tt (next!)
                        test (cond
                               (= [:star] tt) [:any]
                               (and (= :name (first tt)) (= [:op "("] (peek*)) (#{"text" "node"} (second tt)))
                               (do (next!) (expect! [:op ")"]) [(keyword (second tt))])
                               (= :name (first tt)) [:name (second tt)]
                               :else (malformed!))]
                    {:axis axis :test test :preds (predicates)}))))
            (predicates []
              (loop [ps []]
                (if (= [:op "["] (peek*))
                  (do (next!) (let [e (or-expr)] (expect! [:op "]"]) (recur (conj ps e))))
                  ps)))]
      (let [e (or-expr)]
        (when (< @pos (count tokens)) (malformed!))
        e))))

;; ---------------------------------------------------------------------------
;; Evaluation. A node-set is {:nodes [ids in document order]}.

(defn- node-set? [v] (map? v))

(defn- string-value [doc id]
  (let [nd (doc id)]
    (case (:kind nd)
      (:text :attr) (:value nd)
      (apply str (keep (fn [j] (let [x (doc j)] (when (= :text (:kind x)) (:value x))))
                       (range (inc id) (inc (:end nd))))))))

(defn- ->number [s]
  (if-let [m (re-matches #"\s*(-?(?:\d+(?:\.\d*)?|\.\d+))\s*" s)]
    #?(:clj (Double/parseDouble (second m)) :cljs (js/parseFloat (second m)))
    ##NaN))

(defn- number->string [x]
  (cond
    (#?(:clj Double/isNaN :cljs js/isNaN) x) "NaN"
    (#?(:clj Double/isInfinite :cljs #(not (js/isFinite %))) x) (if (pos? x) "Infinity" "-Infinity")
    (and (== x (Math/floor x)) (< (Math/abs (double x)) 1e15)) (str (long x))
    :else (str x)))

(defn- str-of [doc v]
  (cond (node-set? v) (if-let [id (first (:nodes v))] (string-value doc id) "")
        (number? v) (number->string v)
        (boolean? v) (if v "true" "false")
        :else v))

(defn- num-of [doc v]
  (cond (number? v) (double v)
        (boolean? v) (if v 1.0 0.0)
        :else (->number (str-of doc v))))

(defn- bool-of [v]
  (cond (node-set? v) (boolean (seq (:nodes v)))
        (number? v) (not (or (zero? v) (#?(:clj Double/isNaN :cljs js/isNaN) v)))
        (string? v) (pos? (count v))
        :else (boolean v)))

(defn- compare-values [doc op a b]
  (let [rel? (#{"<" "<=" ">" ">="} op)
        num-op (fn [x y] (case op "<" (< x y) "<=" (<= x y) ">" (> x y) ">=" (>= x y) "=" (== x y) "!=" (not (== x y))))
        prim (fn [x y]
               (cond
                 rel? (num-op (num-of doc x) (num-of doc y))
                 (or (boolean? x) (boolean? y)) ((if (= "=" op) = not=) (bool-of x) (bool-of y))
                 (or (number? x) (number? y)) (num-op (num-of doc x) (num-of doc y))
                 :else ((if (= "=" op) = not=) (str-of doc x) (str-of doc y))))
        sv #(string-value doc %)]
    (cond
      (and (node-set? a) (node-set? b)) (boolean (some (fn [x] (some (fn [y] (prim (sv x) (sv y))) (:nodes b))) (:nodes a)))
      (node-set? a) (if (boolean? b) (prim (bool-of a) b) (boolean (some #(prim (sv %) b) (:nodes a))))
      (node-set? b) (if (boolean? a) (prim a (bool-of b)) (boolean (some #(prim a (sv %)) (:nodes b))))
      :else (prim a b))))

(defn- ancestors-of [doc id] (vec (rest (take-while some? (iterate #(:parent (doc %)) id)))))

(defn- axis-nodes
  "The nodes on `axis` from `id`, in the axis's order (reverse axes
   nearest first)."
  [doc id axis]
  (let [nd (doc id)
        attr? #(= :attr (:kind (doc %)))
        descendants #(filterv (complement attr?) (range (inc id) (inc (:end nd))))
        siblings #(if (or (attr? id) (nil? (:parent nd))) [] (:children (doc (:parent nd))))]
    (case axis
      :self [id]
      :child (:children nd [])
      :attribute (:attrs nd [])
      :parent (if-let [p (:parent nd)] [p] [])
      :ancestor (ancestors-of doc id)
      :ancestor-or-self (into [id] (ancestors-of doc id))
      :descendant (descendants)
      :descendant-or-self (into [id] (descendants))
      :following-sibling (vec (rest (drop-while #(not= id %) (siblings))))
      :preceding-sibling (vec (reverse (take-while #(not= id %) (siblings))))
      :following (filterv (complement attr?) (range (inc (:end nd)) (count doc)))
      :preceding (let [anc (set (ancestors-of doc id))]
                   (filterv #(not (or (anc %) (attr? %))) (reverse (range 0 id)))))))

(defn- test? [doc axis [k nm] id]
  (let [nd (doc id)
        principal (if (= axis :attribute) :attr :elem)]
    (case k
      :node true
      :text (= :text (:kind nd))
      :any (= principal (:kind nd))
      :name (and (= principal (:kind nd)) (= nm (:name nd))))))

(declare ev)

(defn- filter-by
  "The nodes of `ids` (in the order positions count) each predicate keeps."
  [doc ids preds]
  (reduce (fn [ids pred]
            (let [size (count ids)]
              (vec (keep-indexed (fn [i id]
                                   (let [v (ev doc {:node id :pos (inc i) :size size} pred)]
                                     (when (if (number? v) (== v (inc i)) (bool-of v)) id)))
                                 ids))))
          ids preds))

(defn- apply-step [doc ids {:keys [axis test preds]}]
  (->> ids
       (mapcat (fn [id] (filter-by doc (filterv #(test? doc axis test %) (axis-nodes doc id axis)) preds)))
       distinct sort vec))

(defn- round-half-up [x] (Math/floor (+ (double x) 0.5)))

(defn- call-fn [doc ctx fname args]
  (let [arg #(ev doc ctx (nth args %))
        ctx-set {:nodes [(:node ctx)]}
        s1 #(if (seq args) (str-of doc (arg 0)) (str-of doc ctx-set))]
    (case fname
      "position" (double (:pos ctx))
      "last" (double (:size ctx))
      "count" (double (count (:nodes (arg 0))))
      "not" (not (bool-of (arg 0)))
      "true" true
      "false" false
      "boolean" (bool-of (arg 0))
      "string" (s1)
      "number" (if (seq args) (num-of doc (arg 0)) (num-of doc ctx-set))
      "concat" (apply str (map #(str-of doc (ev doc ctx %)) args))
      "contains" (str/includes? (str-of doc (arg 0)) (str-of doc (arg 1)))
      "starts-with" (str/starts-with? (str-of doc (arg 0)) (str-of doc (arg 1)))
      "ends-with" (str/ends-with? (str-of doc (arg 0)) (str-of doc (arg 1)))
      "substring-before" (let [s (str-of doc (arg 0)) t (str-of doc (arg 1))]
                           (if-let [i (str/index-of s t)] (subs s 0 i) ""))
      "substring-after" (let [s (str-of doc (arg 0)) t (str-of doc (arg 1))]
                          (if-let [i (str/index-of s t)] (subs s (+ i (count t))) ""))
      "substring" (let [s (str-of doc (arg 0))
                        start (round-half-up (num-of doc (arg 1)))
                        end (if (> (count args) 2) (+ start (round-half-up (num-of doc (arg 2)))) ##Inf)]
                    (apply str (keep-indexed (fn [i ch] (when (and (>= (inc i) start) (< (inc i) end)) ch)) s)))
      "string-length" (double (count (s1)))
      "normalize-space" (str/join " " (remove str/blank? (str/split (s1) #"\s+")))
      "translate" (let [s (str-of doc (arg 0)) from (str-of doc (arg 1)) to (str-of doc (arg 2))
                        m (reduce (fn [m [i ch]] (if (contains? m ch) m (assoc m ch (get to i))))
                                  {} (map-indexed vector from))]
                    (apply str (keep #(if (contains? m %) (get m %) %) s)))
      "sum" (reduce + 0.0 (map #(->number (string-value doc %)) (:nodes (arg 0))))
      "floor" (Math/floor (num-of doc (arg 0)))
      "ceiling" (Math/ceil (num-of doc (arg 0)))
      "round" (round-half-up (num-of doc (arg 0)))
      ("name" "local-name") (let [ids (if (seq args) (:nodes (arg 0)) [(:node ctx)])]
                              (or (some-> (first ids) doc :name) ""))
      (malformed!))))

(defn- ev [doc ctx e]
  (case (first e)
    :num (second e)
    :str (second e)
    :or (or (bool-of (ev doc ctx (nth e 1))) (bool-of (ev doc ctx (nth e 2))))
    :and (and (bool-of (ev doc ctx (nth e 1))) (bool-of (ev doc ctx (nth e 2))))
    :cmp (compare-values doc (nth e 1) (ev doc ctx (nth e 2)) (ev doc ctx (nth e 3)))
    :arith (let [x (num-of doc (ev doc ctx (nth e 2))) y (num-of doc (ev doc ctx (nth e 3)))]
             (case (nth e 1) "+" (+ x y) "-" (- x y) "*" (* x y) "div" (/ x y) "mod" (rem x y)))
    :neg (- (num-of doc (ev doc ctx (nth e 1))))
    :union (let [a (ev doc ctx (nth e 1)) b (ev doc ctx (nth e 2))]
             (when-not (and (node-set? a) (node-set? b)) (malformed!))
             {:nodes (vec (sort (distinct (concat (:nodes a) (:nodes b)))))})
    :path (let [[_ abs? steps] e]
            {:nodes (reduce #(apply-step doc %1 %2) [(if abs? 0 (:node ctx))] steps)})
    :filter (let [[_ p preds steps] e
                  v (ev doc ctx p)]
              (when-not (node-set? v) (malformed!))
              {:nodes (reduce #(apply-step doc %1 %2) (filter-by doc (:nodes v) preds) steps)})
    :call (call-fn doc ctx (nth e 1) (nth e 2))))

(defn- cell-of
  "A node's text as a cell value: a number when it reads as one, as
   Excel's FILTERXML returns them."
  [s]
  (if-let [[_ m] (re-matches #"\s*([+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)\s*" s)]
    (val/number #?(:clj (Double/parseDouble m) :cljs (js/parseFloat m)))
    (val/string s)))

(defn filter-xml
  "FILTERXML's value for texts `xml` and `xpath`."
  [xml xpath]
  (let [doc (parse-xml xml)
        e (when doc (try (parse-xpath xpath)
                         (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) _ nil)))
        v (when e (try (ev doc {:node 0 :pos 1 :size 1} e)
                       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) _ nil)))]
    (cond
      (nil? v) val/ERR-VALUE
      (node-set? v) (let [cells (mapv #(cell-of (string-value doc %)) (:nodes v))]
                      (case (count cells)
                        0 val/ERR-VALUE
                        1 (first cells)
                        {:t :area :r0 0 :c0 0 :r1 (dec (count cells)) :c1 0 :values (mapv vector cells)}))
      (number? v) (if (#?(:clj Double/isNaN :cljs js/isNaN) v) val/ERR-VALUE (val/number v))
      (boolean? v) (val/number (if v 1.0 0.0))
      :else (val/string v))))

(f/register! "FILTERXML"
             (fn [args]
               (filter-xml (f/str! (nth args 0)) (f/str! (nth args 1))))
             :arity [2 2])
