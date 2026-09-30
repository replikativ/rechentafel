(ns rechentafel.fn.numfmt
  "Excel number formats, as TEXT applies them.

   A format has up to four `;`-separated sections — positive; negative;
   zero; text — or conditional sections (`[<3]\"low\"0;[>=10]0;0`): the
   first whose condition holds, a section without one for the rest. A
   negative section formats the absolute value; with one section (or a
   conditional one) the minus sign is automatic. In a section:

     0 # ?        digit placeholders (pad with 0, nothing, a space); extra
                  integer digits go to the leftmost placeholder
     .            the decimal point
     ,            thousands grouping between digits; after the last integer
                  digit, divide by 1000 each
     %            multiply by 100
     E+ E- e+     scientific notation
     \"text\" \\x   literals; _x a space; *x (fill) nothing
     [$€-407]     a currency symbol; [Red] and other colours are dropped
     @            the text, in a text section

   Rounding is half away from zero on the decimal value, as Excel's (not
   the binary half-even of `format`). Date and time formats are
   `date-format`'s (a fn of serial and section), passed in by TEXT."
  (:require [clojure.string :as str]))

(defn split-sections
  "`fmt` split on `;` outside quotes, brackets and escapes."
  [fmt]
  (loop [i 0 cur "" out [] q? false b? false]
    (if (>= i (count fmt))
      (conj out cur)
      (let [c (nth fmt i)]
        (cond
          (and (= c \\) (not q?) (< (inc i) (count fmt)))
          (recur (+ i 2) (str cur c (nth fmt (inc i))) out q? b?)
          (= c \") (recur (inc i) (str cur c) out (not q?) b?)
          (and (= c \[) (not q?)) (recur (inc i) (str cur c) out q? true)
          (and (= c \]) (not q?)) (recur (inc i) (str cur c) out q? false)
          (and (= c \;) (not q?) (not b?)) (recur (inc i) "" (conj out cur) q? b?)
          :else (recur (inc i) (str cur c) out q? b?))))))

(defn- condition-of
  "The `[<op><number>]` condition at the start of `section` (after colour
   brackets): `[op n]`, or nil."
  [section]
  (when-let [[_ op n] (re-find #"^(?:\[[A-Za-z]+\])*\[(<=|>=|<>|<|>|=)\s*(-?[0-9.]+)\]" section)]
    [op #?(:clj (Double/parseDouble n) :cljs (js/parseFloat n))]))

(defn- holds? [[op n] x]
  (case op "<" (< x n) "<=" (<= x n) ">" (> x n) ">=" (>= x n) "=" (== x n) "<>" (not (== x n))))

(defn choose-section
  "The section a number `x` is formatted with: `[section value sign?]`
   (`sign?`: prefix a minus when the value is negative)."
  [sections x]
  (let [conds (mapv condition-of sections)]
    (if (some some? conds)
      (let [hit (some (fn [[s c]] (when (and c (holds? c x)) s)) (map vector sections conds))
            else (some (fn [[s c]] (when-not c s)) (map vector sections conds))]
        [(or hit else "") x true])
      (case (count sections)
        1 [(first sections) x true]
        2 (if (neg? x) [(second sections) (- x) false] [(first sections) x true])
        (cond (pos? x) [(first sections) x true]
              (neg? x) [(second sections) (- x) false]
              :else [(nth sections 2) 0.0 true])))))

(defn tokenize
  "A section as tokens: [:d \\0|\\#|\\?] [:dot] [:comma] [:pct] [:exp \"+\"|\"-\"]
   [:at] [:lit s]."
  [section]
  (let [n (count section)]
    (loop [i 0 out [] dot? false]
      (if (>= i n)
        out
        (let [c (nth section i)]
          (cond
            (= c \") (let [j (or (str/index-of section "\"" (inc i)) n)]
                       (recur (inc j) (conj out [:lit (subs section (inc i) j)]) dot?))
            (= c \\) (recur (+ i 2) (conj out [:lit (subs section (min (inc i) n) (min (+ i 2) n))]) dot?)
            (= c \_) (recur (+ i 2) (conj out [:lit " "]) dot?)
            (= c \*) (recur (+ i 2) out dot?)
            (= c \[) (let [j (or (str/index-of section "]" i) (dec n))
                           inner (subs section (inc i) j)]
                       (recur (inc j)
                              (if-let [[_ sym] (re-find #"^\$([^-]*)" inner)] (conj out [:lit sym]) out)
                              dot?))
            (#{\0 \# \?} c) (recur (inc i) (conj out [:d c]) dot?)
            (and (= c \.) (not dot?)) (recur (inc i) (conj out [:dot]) true)
            (= c \,) (recur (inc i) (conj out [:comma]) dot?)
            (= c \%) (recur (inc i) (conj out [:pct]) dot?)
            (and (#{\E \e} c) (< (inc i) n) (#{\+ \-} (nth section (inc i))))
            (recur (+ i 2) (conj out [:exp (str (nth section (inc i)))]) dot?)
            (= c \@) (recur (inc i) (conj out [:at]) dot?)
            :else (recur (inc i) (conj out [:lit (str c)]) dot?)))))))

(defn- round-decimal
  "`x` (>= 0) rounded half away from zero to `places`, as a plain decimal
   string."
  [x places]
  #?(:clj (-> (BigDecimal. (str x)) (.setScale (int places) java.math.RoundingMode/HALF_UP) .toPlainString)
     :cljs (.toFixed (+ x (* x 1e-15)) places)))

(defn- group3 [digits]
  (->> (reverse digits) (partition-all 3) (map (comp str/join reverse)) reverse (str/join ",")))

(defn format-number
  "Number `x` in number-format `section` (no date codes), with a leading
   minus when `sign?` and `x` is negative at the shown precision."
  [section x sign?]
  (let [toks0 (tokenize section)
        ;; commas right after the last digit placeholder of the number (before
        ;; any exponent) divide by 1000 each: "0.0,," is millions
        exp-at (or (first (keep-indexed (fn [i t] (when (= :exp (first t)) i)) toks0)) (count toks0))
        last-digit (or (last (keep-indexed (fn [i t] (when (and (< i exp-at) (= :d (first t))) i)) toks0)) -1)
        scale (count (take-while #(= [:comma] %) (drop (inc last-digit) toks0)))
        toks (vec (concat (take (inc last-digit) toks0) (drop (+ (inc last-digit) scale) toks0)))
        exp (some #(when (= :exp (first %)) %) toks)
        pcts (count (filter #(= [:pct] %) toks))
        dot-at (or (first (keep-indexed (fn [i t] (when (= [:dot] t) i)) toks)) (count toks))
        [int-toks frac-toks] (split-at dot-at toks)
        frac-toks (rest frac-toks)
        mant-toks (if exp (take-while #(not= :exp (first %)) frac-toks) frac-toks)
        exp-toks (when exp (rest (drop-while #(not= :exp (first %)) frac-toks)))
        ;; commas between integer digits group thousands
        last-d (or (last (keep-indexed (fn [i t] (when (= :d (first t)) i)) int-toks)) -1)
        grouping? (some #(= [:comma] %) (take last-d int-toks))
        int-toks (vec (remove #(= [:comma] %) int-toks))
        int-ph (filter #(= :d (first %)) int-toks)
        frac-ph (filter #(= :d (first %)) mant-toks)
        nfrac (count frac-ph)
        v (* (Math/abs (double x)) (Math/pow 100.0 pcts) (Math/pow 1000.0 (- scale)))
        [v e] (if (and exp (pos? v))
                (let [nint (max 1 (count int-ph))
                      e (- (long (Math/floor (Math/log10 v))) (dec nint))]
                  [(/ v (Math/pow 10.0 e)) e])
                [v 0])
        rounded (round-decimal v nfrac)
        [ip fp] (str/split rounded #"\." 2)
        ;; an exponent's mantissa may round up to another digit
        [ip fp e] (if (and exp (> (count (str/replace ip #"^0+" "")) (max 1 (count int-ph))))
                    (let [v2 (/ v 10.0) [a b] (str/split (round-decimal v2 nfrac) #"\." 2)] [a b (inc e)])
                    [ip fp e])
        int-digits (if (= "0" ip) "" ip)
        minus? (and sign? (neg? (double x)) (or (seq (str/replace (str ip fp) #"0" "")) false))
        ln (count int-digits)
        dc (count int-ph)
        ;; integer part, positionally: digit i of the placeholders shows
        ;; digit (ln - dc + i); the first shows every extra digit
        int-out (loop [ts int-toks i 0 out []]
                  (if-let [[k c :as t] (first ts)]
                    (if (= :d k)
                      (let [ni (+ (- ln dc) i)
                            s (cond
                                (and (zero? i) (pos? ni)) (subs int-digits 0 (inc ni))
                                (>= ni 0) (subs int-digits ni (inc ni))
                                (= c \0) "0" (= c \?) " " :else "")]
                        (recur (rest ts) (inc i) (conj out [:digits s])))
                      (recur (rest ts) i (conj out t)))
                    out))
        int-str (if grouping?
                  ;; grouping over the digits the placeholders show
                  (let [digits (apply str (keep #(when (= :digits (first %)) (second %)) int-out))
                        grouped (group3 (str/replace digits #" " ""))
                        first-d (first (keep-indexed (fn [i t] (when (= :digits (first t)) i)) int-out))]
                    (apply str (map-indexed (fn [i t] (cond (= :digits (first t)) (if (= i first-d) grouped "")
                                                            (= :lit (first t)) (second t) :else ""))
                                            int-out)))
                  (apply str (map (fn [t] (if (#{:digits :lit} (first t)) (second t) "")) int-out)))
        fp (or fp "")
        ;; fraction: trailing zeros under # disappear, under ? become spaces
        last-shown (let [ph (vec frac-ph)]
                     (loop [j (dec nfrac)]
                       (if (and (>= j 0) (not= \0 (second (nth ph j))) (= \0 (nth fp j \0))) (recur (dec j)) j)))
        frac-str (loop [ts mant-toks j 0 out ""]
                   (if-let [[k c :as t] (first ts)]
                     (if (= :d k)
                       (recur (rest ts) (inc j) (str out (cond (<= j last-shown) (nth fp j \0) (= c \?) " " :else "")))
                       (recur (rest ts) j (str out (if (= :lit k) c ""))))
                     out))
        dot (if (< dot-at (count toks)) "." "")
        exp-str (when exp
                  (let [digits (count (filter #(= :d (first %)) exp-toks))
                        s (str (Math/abs (long e)))
                        s (str (apply str (repeat (- digits (count s)) "0")) s)]
                    (str "E" (cond (neg? e) "-" (= "+" (second exp)) "+" :else "") s)))]
    (str (when minus? "-") int-str dot frac-str exp-str
         ;; literals after the digits (a trailing %, text) are in the tokens
         (apply str (keep (fn [t] (case (first t) :pct "%" nil)) toks)))))

(defn date-section? [section date-tokens] (some? (date-tokens section)))

(defn format-value
  "`x` (a number) or `text` (a string) in number format `fmt`. `date-format`
   `(fn [serial section])` renders a date/time section; `date-tokens` tells
   one."
  [fmt {:keys [number text]} {:keys [date-format date-tokens]}]
  (let [sections (split-sections fmt)]
    (if (some? text)
      (let [ts (when (>= (count sections) 4) (nth sections 3))
            ts (or ts (some #(when (str/includes? % "@") %) sections))]
        (if ts
          (apply str (map (fn [[k v]] (case k :at text :lit v "")) (tokenize ts)))
          text))
      (let [[section v sign?] (choose-section sections (double number))
            ;; the condition itself is not part of the section's text
            section (str/replace section #"^((?:\[[A-Za-z]+\])*)\[(?:<=|>=|<>|<|>|=)\s*-?[0-9.]+\]" "$1")]
        (cond
          (str/blank? section) ""
          (re-find #"(?i)^general$" (str/trim section)) nil
          (date-tokens section) (date-format v section)
          :else (format-number section (if sign? (double number) v) sign?))))))
