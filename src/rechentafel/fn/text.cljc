(ns rechentafel.fn.text
  "Text functions (POI category: text — 38 fns, of which ~14 are
  explicitly NotImplementedFunction in POI itself — ASC/DBCS/JIS and the
  LENB/LEFTB/MIDB/RIGHTB/FINDB/SEARCHB/REPLACEB byte-oriented siblings
  only relevant to double-byte locales). We register the implemented
  ones plus the DBCS group as stubs that delegate to the single-byte
  version (matches how most real-world Excel formulas use them).

  POI's TextFunction base class coerces every arg through OperandResolver
  to a string; blanks become \"\", booleans become \"TRUE\"/\"FALSE\",
  numbers use Excel's default number format. `val/to-str` follows the
  same rules."
  (:require [clojure.string :as str]
            [cljc.java-time.day-of-week :as dow]
            [cljc.java-time.local-date-time :as ldt]
            [rechentafel.fn.datetime :as datetime]
            [rechentafel.platform :as p]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]
            [rechentafel.fn.numfmt :as numfmt]))

(defn- letter? [c]
  #?(:clj  (Character/isLetter ^char c)
     :cljs (boolean (re-matches #"[A-Za-z]" (str c)))))

(defn- digit? [c]
  #?(:clj  (Character/isDigit ^char c)
     :cljs (boolean (re-matches #"\d" (str c)))))

;; ---------------------------------------------------------------------------
;; Case + whitespace

(f/register! "UPPER"
             ^{:scalar? true}
             (fn [args] (val/string (str/upper-case (f/str! (nth args 0)))))
             :arity [1 1])

(f/register! "LOWER"
             ^{:scalar? true}
             (fn [args] (val/string (str/lower-case (f/str! (nth args 0)))))
             :arity [1 1])

(defn- proper-case [^String s]
  (let [n  (count s)
        sb (p/sb)]
    (loop [i 0, cap? true]
      (if (= i n)
        (p/sb->str sb)
        (let [c   (subs s i (inc i))
              ltr (letter? (.charAt s i))
              c2 (cond
                   (not ltr) c
                   cap?      (str/upper-case c)
                   :else     (str/lower-case c))]
          (p/sb-append! sb c2)
          (recur (inc i) (not ltr)))))))

(f/register! "PROPER"
             ^{:scalar? true}
             (fn [args] (val/string (proper-case (f/str! (nth args 0)))))
             :arity [1 1])

(f/register! "TRIM"
  ;; Excel TRIM: strip leading/trailing spaces, collapse internal runs to a
  ;; single space. Only U+0020 (not general whitespace).
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))]
                 (val/string (-> s
                                 (str/replace #" +" " ")
                                 (str/replace #"^ +| +$" "")))))
             :arity [1 1])

(f/register! "CLEAN"
  ;; Strip all non-printable (< 0x20) characters.
             ^{:scalar? true}
             (fn [args]
               (let [s  (f/str! (nth args 0))
                     sb (p/sb)]
                 (doseq [c s]
                   (when (>= #?(:clj (int c) :cljs (.charCodeAt c 0)) 32)
                     (p/sb-append! sb c)))
                 (val/string (p/sb->str sb))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Length / char / code

(f/register! "LEN"
             ^{:scalar? true}
             (fn [args] (val/number (count (f/str! (nth args 0)))))
             :arity [1 1])

(f/register! "LENB"  ;; stub: double-byte-aware length — use LEN semantics.
             ^{:scalar? true}
             (fn [args] (val/number (count (f/str! (nth args 0)))))
             :arity [1 1])

(f/register! "CHAR"
             ^{:scalar? true}
             (fn [args]
               (let [n (long (f/num! (nth args 0)))]
                 (when (or (< n 1) (> n 255)) (f/domain-error! :value))
                 (val/string (str (char n)))))
             :arity [1 1])

(f/register! "CODE"
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))]
                 (when (zero? (count s)) (f/domain-error! :value))
                 (val/number #?(:clj  (int (.charAt ^String s 0))
                                :cljs (.charCodeAt s 0)))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; LEFT / RIGHT / MID

(defn- clamp-len ^long [^long n]
  (cond (neg? n)  -1
        :else     n))

(f/register! "LEFT"
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))
                     n (if (> (count args) 1) (long (f/num! (nth args 1))) 1)]
                 (when (neg? n) (f/domain-error! :value))
                 (val/string (subs s 0 (min (count s) n)))))
             :arity [1 2])

(f/register! "LEFTB" (:fn (f/lookup "LEFT")) :arity [1 2])

(f/register! "RIGHT"
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))
                     n (if (> (count args) 1) (long (f/num! (nth args 1))) 1)
                     len (count s)]
                 (when (neg? n) (f/domain-error! :value))
                 (val/string (subs s (max 0 (- len n)) len))))
             :arity [1 2])

(f/register! "RIGHTB" (:fn (f/lookup "RIGHT")) :arity [1 2])

(f/register! "MID"
  ;; MID(text, start_num, num_chars)
  ;; start < 1 → #VALUE!; start > len → ""; num_chars < 0 → #VALUE!.
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))
                     start (long (f/num! (nth args 1)))
                     len (long (f/num! (nth args 2)))
                     slen (count s)]
                 (when (< start 1) (f/domain-error! :value))
                 (when (neg? len)  (f/domain-error! :value))
                 (if (> start slen) (val/string "")
                     (let [s0 (dec start)]
                       (val/string (subs s s0 (min slen (+ s0 len))))))))
             :arity [3 3])

(f/register! "MIDB" (:fn (f/lookup "MID")) :arity [3 3])

;; ---------------------------------------------------------------------------
;; FIND / SEARCH

(f/register! "FIND"
  ;; Case-sensitive, no wildcards. Start defaults to 1. Returns 1-based index.
             ^{:scalar? true}
             (fn [args]
               (let [needle (f/str! (nth args 0))
                     hay    (f/str! (nth args 1))
                     start  (if (> (count args) 2) (long (f/num! (nth args 2))) 1)]
                 (when (< start 1) (f/domain-error! :value))
                 (let [idx (.indexOf ^String hay ^String needle (dec start))]
                   (if (neg? idx) (f/domain-error! :value)
                       (val/number (inc idx))))))
             :arity [2 3])

(f/register! "FINDB" (:fn (f/lookup "FIND")) :arity [2 3])

(defn- wildcard->re
  "Excel SEARCH wildcards: `?` matches any 1 char, `*` matches any run.
  Literal `?`/`*` are `~?` / `~*`. Tildes themselves are `~~`."
  ^String [^String s]
  (let [sb (p/sb)
        n  (count s)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt s i)]
          (cond
            (and (= c \~) (< (inc i) n))
            (let [nxt (.charAt s (inc i))]
              (p/sb-append! sb (p/regex-quote (str nxt)))
              (recur (+ i 2)))
            (= c \?) (do (p/sb-append! sb ".") (recur (inc i)))
            (= c \*) (do (p/sb-append! sb ".*") (recur (inc i)))
            :else    (do (p/sb-append! sb (p/regex-quote (str c)))
                         (recur (inc i)))))))
    (p/sb->str sb)))

(f/register! "SEARCH"
  ;; Case-insensitive, supports ?/* wildcards.
             ^{:scalar? true}
             (fn [args]
               (let [needle (f/str! (nth args 0))
                     hay    (f/str! (nth args 1))
                     start  (if (> (count args) 2) (long (f/num! (nth args 2))) 1)]
                 (when (< start 1) (f/domain-error! :value))
                 (let [re  (p/pattern-icase (wildcard->re needle))
                       off (dec start)
                       sub (subs hay off)]
                   #?(:clj  (let [m (.matcher ^java.util.regex.Pattern re ^String sub)]
                              (if (.find m)
                                (val/number (+ start (.start m)))
                                (f/domain-error! :value)))
                      :cljs (let [m (.match sub re)]
                              (if m
                                (val/number (+ start (.-index m)))
                                (f/domain-error! :value)))))))
             :arity [2 3])

(f/register! "SEARCHB" (:fn (f/lookup "SEARCH")) :arity [2 3])

;; ---------------------------------------------------------------------------
;; EXACT / SUBSTITUTE / REPLACE / REPT

(f/register! "EXACT"
             ^{:scalar? true}
             (fn [args]
               (val/boolean-v (= (f/str! (nth args 0)) (f/str! (nth args 1)))))
             :arity [2 2])

(f/register! "SUBSTITUTE"
  ;; SUBSTITUTE(text, old, new, [instance_num])
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))
                     o (f/str! (nth args 1))
                     n (f/str! (nth args 2))
                     inst (if (> (count args) 3)
                            (long (f/num! (nth args 3)))
                            0)]
                 (cond
                   (empty? o) (val/string s)
                   (zero? inst) (val/string (str/replace s o n))
                   (neg? inst) (f/domain-error! :value)
                   :else
                   (let [olen (count o)]
                     (loop [i 0, out (p/sb), k 1]
                       (let [idx (.indexOf ^String s ^String o (int i))]
                         (cond
                           (neg? idx)
                           (val/string (-> out (p/sb-append! (subs s i)) p/sb->str))
                           (= k inst)
                           (val/string (-> out
                                           (p/sb-append! (subs s i idx))
                                           (p/sb-append! n)
                                           (p/sb-append! (subs s (+ idx olen)))
                                           p/sb->str))
                           :else
                           (recur (+ idx olen)
                                  (-> out
                                      (p/sb-append! (subs s i idx))
                                      (p/sb-append! o))
                                  (inc k)))))))))
             :arity [3 4])

(f/register! "REPLACE"
  ;; REPLACE(text, start_num, num_chars, new_text)
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))
                     start (long (f/num! (nth args 1)))
                     n    (long (f/num! (nth args 2)))
                     ins  (f/str! (nth args 3))
                     slen (count s)]
                 (when (< start 1) (f/domain-error! :value))
                 (when (neg? n) (f/domain-error! :value))
                 (let [a (min slen (dec start))
                       b (min slen (+ a n))]
                   (val/string (str (subs s 0 a) ins (subs s b))))))
             :arity [4 4])

(f/register! "REPLACEB" (:fn (f/lookup "REPLACE")) :arity [4 4])

(f/register! "REPT"
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))
                     n (long (f/num! (nth args 1)))]
                 (when (neg? n) (f/domain-error! :value))
      ;; Excel caps output at 32767 chars.
                 (when (> (* (count s) n) 32767) (f/domain-error! :value))
                 (val/string (str/join (repeat n s)))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; CONCAT / CONCATENATE / TEXTJOIN

(defn- concat-strs ^String [args]
  (let [sb (p/sb)]
    (f/each-scalar
     args
     (fn [v _in-area?]
       (case (:t v)
         :err  (f/domain-error! (:v v))
         :blank nil
         (let [s (val/to-str v)]
           (cond
             (val/str? s) (p/sb-append! sb (:v s))
             (val/err? s) (f/domain-error! (:v s))
             :else        (f/domain-error! :value))))))
    (p/sb->str sb)))

(f/register! "CONCATENATE"
             (fn [args] (val/string (concat-strs args)))
             :arity [1 nil])

(f/register! "CONCAT" (:fn (f/lookup "CONCATENATE")) :arity [1 nil])

(f/register! "TEXTJOIN"
  ;; TEXTJOIN(delimiter, ignore_empty, text1, ...)
             (fn [args]
               (let [delim (f/str! (nth args 0))
                     ;; left empty (TEXTJOIN(",",,…)) Excel ignores empty
                     ;; values, as its saved results show
                     ignore-empty? (if (val/blank? (nth args 1)) true (f/bool! (nth args 1)))
                     rest-args (subvec (vec args) 2)
                     parts (volatile! (transient []))]
                 (f/each-scalar
                  rest-args
                  (fn [v _in-area?]
                    (case (:t v)
                      :err   (f/domain-error! (:v v))
                      :blank (when-not ignore-empty? (vswap! parts conj! ""))
                      (let [s (val/to-str v)]
                        (cond
                          (val/err? s) (f/domain-error! (:v s))
                          (val/str? s) (let [sv (:v s)]
                                         (when-not (and ignore-empty? (empty? sv))
                                           (vswap! parts conj! sv)))
                          :else (f/domain-error! :value))))))
                 (val/string (str/join delim (persistent! @parts)))))
             :arity [3 nil])

;; ---------------------------------------------------------------------------
;; T / N — scalar-type gates

(f/register! "T"
  ;; Returns the text if the value is text, else \"\" (per Excel).
             ^{:scalar? true}
             (fn [args]
               (let [v (nth args 0)]
                 (case (:t v)
                   :str   v
                   :err   v
                   (val/string ""))))
             :arity [1 1])

(f/register! "N"
  ;; Coerce to number; non-numeric non-error → 0.
             ^{:scalar? true}
             (fn [args]
               (let [v (nth args 0)]
                 (case (:t v)
                   :num   v
                   :bool  (val/number (if (:v v) 1.0 0.0))
                   :blank (val/number 0.0)
                   :err   v
                   (val/number 0.0))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; VALUE / NUMBERVALUE / DOLLAR / FIXED / TEXT

(def ^:private min-thousand-gap
  "POI's rule: thousands separators must sit at least 4 characters apart."
  4)

(defn- parse-excel-number
  "Port of POI's Value.convertTextToNumber. Returns a double or nil.
  Supports:
    - leading/trailing whitespace
    - optional $ currency symbol
    - optional unary +/-
    - thousands separators (comma) with the 3-digit spacing rule
    - decimal point
    - scientific notation (e/E)
    - trailing %"
  [^String raw]
  (when-let [s (and raw (not-empty (str/trim raw)))]
    (let [len (count s)]
      (loop [i 0
             found-currency? false
             found-plus?     false
             found-minus?    false]
        (if (>= i len)
          (when-not (or found-currency? found-minus? found-plus?) 0.0)
          (let [ch (.charAt s i)]
            (cond
              (or (digit? ch) (= ch \.))
              ;; switch into digit-consumption mode
              (let [sb (p/sb)]
                (loop [j i
                       last-thou -1000
                       found-dot? false
                       found-pct? false]
                  (if (>= j len)
                    (if (and (not found-dot?)
                             (< (- j last-thou) min-thousand-gap))
                      nil
                      (when-let [d (p/parse-double (p/sb->str sb))]
                        (let [d (if found-minus? (- d) d)]
                          (if found-pct? (/ d 100.0) d))))
                    (let [c (.charAt s j)]
                      (cond
                        (digit? c)
                        (do (p/sb-append! sb c)
                            (recur (inc j) last-thou found-dot? found-pct?))

                        (= c \space)
                        (let [rest-trimmed (str/trim (subs s j))]
                          (cond
                            (= rest-trimmed "%")
                            (recur len last-thou found-dot? true)
                            (empty? rest-trimmed)
                            (recur len last-thou found-dot? found-pct?)
                            :else nil))

                        (= c \.)
                        (if (or found-dot?
                                (< (- j last-thou) min-thousand-gap))
                          nil
                          (do (p/sb-append! sb \.)
                              (recur (inc j) last-thou true found-pct?)))

                        (= c \,)
                        (if (or found-dot?
                                (< (- j last-thou) min-thousand-gap))
                          nil
                          (recur (inc j) j found-dot? found-pct?))

                        (or (= c \e) (= c \E))
                        (if (< (- j last-thou) min-thousand-gap)
                          nil
                          (do (p/sb-append! sb (subs s j))
                              (recur len last-thou found-dot? found-pct?)))

                        (= c \%)
                        (recur (inc j) last-thou found-dot? true)

                        :else nil)))))

              (= ch \space)
              (recur (inc i) found-currency? found-plus? found-minus?)

              (= ch \$)
              (if found-currency?
                nil
                (recur (inc i) true found-plus? found-minus?))

              (= ch \+)
              (if (or found-plus? found-minus?)
                nil
                (recur (inc i) found-currency? true found-minus?))

              (= ch \-)
              (if (or found-plus? found-minus?)
                nil
                (recur (inc i) found-currency? found-plus? true))

              :else nil)))))))

(f/register! "VALUE"
  ;; VALUE(text) — number parse matching POI/Excel. "" → 0; blanks → #VALUE!.
             ^{:scalar? true}
             (fn [args]
               (let [v (nth args 0)]
                 (case (:t v)
                   :num v
                   :blank (val/number 0.0)
                   :bool  (val/number (if (:v v) 1.0 0.0))
                   :err   v
                   :str   (let [raw (:v v)]
                            (if (str/blank? raw)
                              (if (= "" raw) (val/number 0.0) (f/domain-error! :value))
                              (if-let [d (parse-excel-number raw)]
                                (val/number d)
                                (f/domain-error! :value))))
                   (f/domain-error! :value))))
             :arity [1 1])

(f/register! "NUMBERVALUE"
  ;; NUMBERVALUE(text, [decimal_separator], [group_separator])
  ;; We stage the replacement through a placeholder so that dec_sep and
  ;; group_sep sharing a character (or a substring) can't mangle each other.
             ^{:scalar? true}
             (fn [args]
               (let [s     (f/str! (nth args 0))
                     dec-s (if (> (count args) 1) (f/str! (nth args 1)) ".")
                     grp-s (if (> (count args) 2) (f/str! (nth args 2)) ",")
                     placeholder "\u0001"
                     cleaned (-> s
                                 (str/replace dec-s placeholder)
                                 (str/replace grp-s "")
                                 (str/replace placeholder "."))
                     n (p/parse-double cleaned)]
                 (if n (val/number n) (f/domain-error! :value))))
             :arity [1 3])

(defn- round-half-even
  "Banker's rounding — half-to-even, matching Java `Math/rint` and
  Java's BigDecimal HALF_EVEN (which is what POI's DataFormatter
  uses for FIXED/DOLLAR/TEXT). JS lacks Math.rint, so we emulate."
  ^double [^double x]
  #?(:clj  (Math/rint x)
     :cljs (let [r (Math/round x)
                 frac (- x (Math/floor x))]
             (if (and (== 0.5 (Math/abs (- (Math/abs x)
                                           (Math/floor (Math/abs x)))))
                      (odd? r))
               (- r 1)
               r))))

(defn- format-fixed [^double n ^long digits comma?]
  (let [scale (Math/pow 10.0 digits)
        r (/ (round-half-even (* n scale)) scale)
        formatted #?(:clj (format (str "%." (max 0 digits) "f") r)
                     :cljs (.toFixed r (max 0 digits)))
        [intp decp] (str/split formatted #"\." 2)
        neg? (str/starts-with? intp "-")
        abs-int (if neg? (subs intp 1) intp)
        grouped (if (and comma? (>= (count abs-int) 4))
                  (->> (reverse abs-int)
                       (partition-all 3)
                       (map (comp str/join reverse))
                       reverse
                       (str/join ","))
                  abs-int)
        sign (if neg? "-" "")]
    (if decp (str sign grouped "." decp) (str sign grouped))))

(f/register! "FIXED"
  ;; FIXED(number, [decimals=2], [no_commas=FALSE])
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     d (if (> (count args) 1) (long (f/num! (nth args 1))) 2)
                     no-commas? (if (> (count args) 2) (f/bool! (nth args 2)) false)]
                 (val/string (format-fixed x d (not no-commas?)))))
             :arity [1 3])

(f/register! "DOLLAR"
  ;; DOLLAR(number, [decimals=2])
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     d (if (> (count args) 1) (long (f/num! (nth args 1))) 2)
                     body (format-fixed (Math/abs x) d true)]
                 (val/string
                  (if (neg? x)
                    (str "($" body ")")
                    (str "$" body)))))
             :arity [1 2])

(def ^:private month-names
  ["January" "February" "March" "April" "May" "June" "July" "August" "September"
   "October" "November" "December"])

(def ^:private day-names ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(defn- date-tokens
  "An Excel date/time format as tokens: [:lit s] or [:y n] [:m n] [:d n]
   [:h n] [:min n] [:s n] [:ampm \"AM/PM\"|\"A/P\"]; nil when it has no date
   or time part (a numeric format). Case-insensitive, as Excel; `[$-409]`
   locale prefixes are dropped; \"quoted\" and \\x are literals; m is minutes
   right after an hour or right before seconds."
  [fmt]
  (let [fmt (str/replace fmt #"\[\$[^\]]*\]" "")
        n (count fmt)
        raw (loop [i 0 out []]
              (if (>= i n)
                out
                (let [c (nth fmt i) lc (str/lower-case (str c))]
                  (cond
                    (= c \") (let [j (or (str/index-of fmt "\"" (inc i)) n)]
                               (recur (inc j) (conj out [:lit (subs fmt (inc i) j)])))
                    (= c \\) (recur (+ i 2) (conj out [:lit (subs fmt (min (inc i) n) (min (+ i 2) n))]))
                    (str/starts-with? (str/lower-case (subs fmt i)) "am/pm") (recur (+ i 5) (conj out [:ampm "AM/PM"]))
                    (str/starts-with? (str/lower-case (subs fmt i)) "a/p") (recur (+ i 3) (conj out [:ampm "A/P"]))
                    (#{"y" "m" "d" "h" "s"} lc)
                    (let [j (loop [j i] (if (and (< j n) (= lc (str/lower-case (str (nth fmt j))))) (recur (inc j)) j))]
                      (recur j (conj out [(keyword lc) (- j i)])))
                    :else (recur (inc i) (conj out [:lit (str c)]))))))
        fields (filterv #(#{:y :m :d :h :s} (first %)) raw)]
    (when (seq fields)
      ;; m after h, or before s, is minutes
      (let [idx (vec (keep-indexed (fn [i t] (when (#{:y :m :d :h :s} (first t)) i)) raw))]
        (reduce (fn [toks k]
                  (let [i (nth idx k)
                        prev (when (pos? k) (first (nth raw (nth idx (dec k)))))
                        nxt (when (< (inc k) (count idx)) (first (nth raw (nth idx (inc k)))))]
                    (if (and (= :m (first (nth raw i))) (or (= :h prev) (= :s nxt)))
                      (assoc toks i [:min (second (nth raw i))])
                      toks)))
                raw (range (count idx)))))))

(defn- pad2 [n] (if (< n 10) (str "0" n) (str n)))

(defn- format-date
  "Serial number `x` in the tokens of `date-tokens`."
  [x toks]
  (let [dt (datetime/serial->datetime x)
        h24 (ldt/get-hour dt)
        ampm? (some #(= :ampm (first %)) toks)
        h (if ampm? (let [h (mod h24 12)] (if (zero? h) 12 h)) h24)]
    (apply str
           (for [[k v] toks]
             (case k
               :lit v
               :y (let [y (ldt/get-year dt)] (if (<= v 2) (pad2 (mod y 100)) (str y)))
               :m (let [m (ldt/get-month-value dt)]
                    (case v 1 (str m) 2 (pad2 m) 3 (subs (month-names (dec m)) 0 3)
                          5 (subs (month-names (dec m)) 0 1) (month-names (dec m))))
               :d (let [d (ldt/get-day-of-month dt)
                        wd (day-names (dec (dow/get-value (ldt/get-day-of-week dt))))]
                    (case v 1 (str d) 2 (pad2 d) 3 (subs wd 0 3) wd))
               :h (if (>= v 2) (pad2 h) (str h))
               :min (let [m (ldt/get-minute dt)] (if (>= v 2) (pad2 m) (str m)))
               :s (let [s (ldt/get-second dt)] (if (>= v 2) (pad2 s) (str s)))
               :ampm (if (= "AM/PM" v) (if (< h24 12) "AM" "PM") (if (< h24 12) "A" "P")))))))

(f/register! "TEXT"
  ;; Numeric/text formatter — full Excel format codes are massive. Start
  ;; with the pieces POI's TextFunction.TEXT actually delegates:
  ;;   0 / #       digit placeholders
  ;;   .           decimal point
  ;;   ,           thousands separator
  ;;   %           percentage (multiply by 100)
  ;;   @           literal text of the arg
  ;; and date/time codes (see `date-tokens`): TEXT(DATE(2020,5,1),"mmmm")
  ;; is "May".
             ^{:scalar? true}
             (fn [args]
               (let [v   (nth args 0)
                     fmt (f/str! (nth args 1))
                     x   (val/to-num v)]
                 (cond
                   (val/err? v) (f/domain-error! (:v v))
                   ;; text that is not a number goes through the text section
                   (not (val/num? x))
                   (val/string (numfmt/format-value fmt {:text (:v (val/to-str v))}
                                                    {:date-tokens date-tokens :date-format (fn [_ _] "")}))
                   :else
                   (let [n (double (:v x))
                         out (numfmt/format-value
                              fmt {:number n}
                              {:date-tokens (fn [section] (and (not (re-find #"(?i)^general$" section)) (date-tokens section)))
                               :date-format (fn [serial section]
                                              (if (neg? (double serial))
                                                (f/domain-error! :value)
                                                (format-date (double serial) (date-tokens section))))})]
                     ;; General: the number as a cell shows it
                     (val/string (or out (:v (val/to-str x)) ""))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; Double-byte / locale stubs — fall back to the single-byte sibling.

(f/register! "ASC"
             ^{:scalar? true}
             (fn [args] (val/string (f/str! (nth args 0)))) :arity [1 1])

(f/register! "DBCS"
             ^{:scalar? true}
             (fn [args] (val/string (f/str! (nth args 0)))) :arity [1 1])

(f/register! "JIS"
             ^{:scalar? true}
             (fn [args] (val/string (f/str! (nth args 0)))) :arity [1 1])

;; ---------------------------------------------------------------------------
;; Unicode helpers (Excel 2013+).

(f/register! "UNICODE"
  ;; Codepoint of the first character. Properly handles surrogate pairs.
             ^{:scalar? true}
             (fn [args]
               (let [s (f/str! (nth args 0))]
                 (when (zero? (count s)) (f/domain-error! :value))
                 (val/number (double (.codePointAt ^String s 0)))))
             :arity [1 1])

(f/register! "UNICHAR"
  ;; Returns the character for a Unicode codepoint (may be 2 UTF-16 chars).
             ^{:scalar? true}
             (fn [args]
               (let [cp (long (f/num! (nth args 0)))]
                 (when (or (<= cp 0) (> cp 0x10FFFF)) (f/domain-error! :value))
                 (val/string (p/char-from-codepoint cp))))
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; TEXTBEFORE / TEXTAFTER / TEXTSPLIT (Excel 365).
;;
;; Signature (simplified):
;;   TEXTBEFORE(text, delim, [instance=1], [match-mode=0], [match-end=0], [if-not-found=#N/A])
;;   TEXTAFTER(text, delim, [instance=1], [match-mode=0], [match-end=0], [if-not-found=#N/A])
;; match-mode: 0 = case-sensitive (default), 1 = case-insensitive.
;; instance: negative means count from the end.
;; We accept delim as a single string (the full Excel variant accepts an
;; array of delimiters; not modelled here since arrays aren't first-class).

(defn- find-all [^String s ^String d case-insensitive?]
  (when (pos? (count d))
    (let [hay (if case-insensitive? (str/lower-case s) s)
          ndl (if case-insensitive? (str/lower-case d) d)]
      (loop [from 0, acc (transient [])]
        (let [i (.indexOf ^String hay ^String ndl (int from))]
          (if (neg? i)
            (persistent! acc)
            (recur (+ i (count ndl)) (conj! acc i))))))))

(defn- text-before-after [mode]
  (with-meta
    (fn [args]
      (let [s      (f/str! (nth args 0))
            d      (f/str! (nth args 1))
            inst   (if (> (count args) 2) (long (f/num! (nth args 2))) 1)
            ci?    (and (> (count args) 3)
                        (not (zero? (long (f/num! (nth args 3))))))
            m-end? (and (> (count args) 4)
                        (not (zero? (long (f/num! (nth args 4))))))
            not-found (if (> (count args) 5) (nth args 5) val/ERR-NA)
            positions (find-all s d ci?)
            n (count positions)]
        (cond
          (zero? n)     (if (map? not-found) not-found (val/string (f/str! not-found)))
          (zero? inst)  (f/domain-error! :value)
          :else
          (let [idx (if (pos? inst) (dec inst) (+ n inst))
                pos (and (<= 0 idx (dec n)) (nth positions idx))]
            (cond
              (nil? pos) (if (map? not-found) not-found (val/string (f/str! not-found)))
              (= mode :before)
              (val/string (subs s 0 pos))
              :else  ;; :after
              (val/string
               (if m-end?
                 (subs s pos)
                 (subs s (+ pos (count d))))))))))
    {:scalar? true}))

(f/register! "TEXTBEFORE" (text-before-after :before) :arity [2 6])
(f/register! "TEXTAFTER"  (text-before-after :after)  :arity [2 6])

(f/register! "TEXTSPLIT"
  ;; TEXTSPLIT(text, col_delimiter, [row_delimiter], [ignore_empty],
  ;;           [match_mode], [pad_with]) — an array: rows split by
  ;; row_delimiter, each row by col_delimiter; a delimiter may be an array
  ;; of strings (any of them splits); match_mode 1 ignores case; ragged rows
  ;; are padded with pad_with (#N/A by default).
             (fn [args]
               (let [arg (fn [i] (when (> (count args) i) (nth args i)))
                     given? (fn [i] (and (arg i) (not (val/blank? (arg i)))))
                     delims (fn [v] (when v
                                      (vec (remove empty? (map #(:v (val/to-str %))
                                                               (if (f/area? v) (apply concat (:values v)) [v]))))))
                     s (f/str! (nth args 0))
                     cols-d (delims (arg 1))
                     rows-d (when (given? 2) (delims (arg 2)))
                     ignore? (and (given? 3) (val/truthy? (arg 3)))
                     ci? (and (given? 4) (= 1 (long (f/num! (arg 4)))))
                     pad (if (given? 5) (arg 5) val/ERR-NA)
                     splitter (fn [ds]
                                (if (seq ds)
                                  (re-pattern (str (when ci? "(?i)") (str/join "|" (map p/regex-quote ds))))
                                  nil))
                     split (fn [text re] (if re (str/split text re -1) [text]))
                     rows (split s (splitter rows-d))
                     rows (if ignore? (remove empty? rows) rows)
                     cells (mapv (fn [r] (let [cs (split r (splitter cols-d))]
                                           (vec (if ignore? (remove empty? cs) cs))))
                                 rows)
                     cells (if (seq cells) cells [[""]])
                     width (apply max 1 (map count cells))]
                 {:t :area :r0 0 :c0 0 :r1 (dec (count cells)) :c1 (dec width)
                  :values (mapv (fn [row] (vec (concat (map val/string row) (repeat (- width (count row)) pad)))) cells)}))
             :arity [2 6])
