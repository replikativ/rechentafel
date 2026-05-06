(ns rechentafel.platform
  "Cross-platform helpers for the things Clojure and ClojureScript spell
  differently. The interpreter targets both runtimes; rather than
  scatter `#?(:clj ... :cljs ...)` reader conditionals across every fn
  module, we centralise them here.

  Anything that has a portable Clojure form (e.g. `clojure.string`
  ops, `Math/abs`, `Math/PI`, `##NaN`/`##Inf` literals) goes directly
  in the call site — no helper needed. We only wrap the genuinely
  different pieces:

    parse-double / parse-long  — string → number
    finite? / nan?              — predicates that diverge on cljs
    regex-quote                 — escape regex metacharacters
    pattern-icase               — compile a case-insensitive regex
    char-from-codepoint         — codepoint → 1-char string
    str-buffer / sb-append /
    sb-toString                 — portable mutable string buffer"
  (:refer-clojure :exclude [parse-double])
  (:require [clojure.string :as str])
  #?(:cljs (:require [goog.string :as gstring])))

;; ---------------------------------------------------------------------------
;; Number parsing

(defn parse-double
  "String → double, or nil on failure. Treats only purely-numeric input
  as a number (Excel's NUMBERVALUE / VALUE functions handle locales
  themselves)."
  [^String s]
  #?(:clj  (try (Double/parseDouble s) (catch Throwable _ nil))
     :cljs (let [n (js/parseFloat s)]
             (when (and (not (js/isNaN n))
                        ;; js/parseFloat is permissive — "12abc" → 12.
                        ;; Reject anything that doesn't fully parse.
                        (re-matches #"\s*-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?\s*"
                                    s))
               n))))

;; parse-long is in clojure.core / cljs.core natively (1.11+) — use it
;; directly; no need for a wrapper.

;; ---------------------------------------------------------------------------
;; Float predicates

(defn finite? [x]
  #?(:clj  (Double/isFinite (double x))
     :cljs (js/isFinite x)))

(defn nan? [x]
  #?(:clj  (Double/isNaN (double x))
     :cljs (js/isNaN x)))

;; ---------------------------------------------------------------------------
;; Regex helpers

(def ^:private regex-meta-re #"[.\^$*+?()\[\]{}|\\]")

(defn regex-quote
  "Escape the regex metacharacters in `s` so it can be embedded in a
  regex. Equivalent to `java.util.regex.Pattern/quote` but without the
  `\\Q…\\E` wrapper (the wrapper isn't supported on cljs's RE2-like
  engine)."
  [^String s]
  (str/replace s regex-meta-re #(str "\\" %)))

(defn pattern-icase
  "Compile `pattern-string` as a case-insensitive regex. JVM uses
  `(?i)` flag prefix; cljs uses the JS RegExp `i` flag."
  [^String pattern-string]
  #?(:clj  (java.util.regex.Pattern/compile (str "(?i)" pattern-string))
     :cljs (js/RegExp. pattern-string "i")))

;; ---------------------------------------------------------------------------
;; Codepoint to string

(defn char-from-codepoint
  "Return a one-character string for the given Unicode code point.
  Handles supplementary plane characters (cp ≥ 0x10000)."
  [cp]
  #?(:clj  (String. (Character/toChars (int cp)))
     :cljs (.fromCodePoint js/String cp)))

;; ---------------------------------------------------------------------------
;; Mutable string buffer
;;
;; JVM:  java.lang.StringBuilder
;; cljs: goog.string.StringBuffer (.append, .toString)

(defn sb
  "Create a fresh mutable string buffer."
  []
  #?(:clj  (StringBuilder.)
     :cljs (gstring/StringBuffer.)))

(defn sb-append!
  "Mutate `buf` by appending `x` (any value coerced via str). Returns buf."
  [buf x]
  #?(:clj  (.append ^StringBuilder buf (str x))
     :cljs (.append ^js buf x))
  buf)

(defn sb->str
  "Materialise the buffer as a String."
  ^String [buf]
  #?(:clj  (.toString ^StringBuilder buf)
     :cljs (.toString ^js buf)))

;; ---------------------------------------------------------------------------
;; Mutable numeric / generic arrays (used by MTV column-block storage)
;;
;; JVM:  primitive double[] / Object[]
;; cljs: js/Float64Array  / js/Array

(defn make-num-array [^long n]
  #?(:clj  (double-array n)
     :cljs (js/Float64Array. n)))

(defn make-gen-array [^long n]
  #?(:clj  (object-array n)
     :cljs (js/Array. n)))

(defn doubles-from
  "Coerce a seqable of numbers into a mutable num-array."
  [xs]
  #?(:clj  (double-array xs)
     :cljs (let [n  (count xs)
                 a  (js/Float64Array. n)
                 it (vec xs)]
             (dotimes [i n] (aset a i (double (nth it i))))
             a)))

(defn objects-from
  "Coerce a seqable into a mutable gen-array."
  [xs]
  #?(:clj  (object-array xs)
     :cljs (into-array xs)))

(defn arr-len ^long [arr]
  #?(:clj  (alength arr)
     :cljs (.-length arr)))

(defn arr-copy!
  "Copy `len` elements from `src` starting at `src-off` into `dst`
  starting at `dst-off`. `src` and `dst` must be the same kind (both
  num-arrays or both gen-arrays). Returns dst."
  [src src-off dst dst-off len]
  #?(:clj  (System/arraycopy src src-off dst dst-off len)
     :cljs (loop [i 0]
             (when (< i len)
               (aset dst (+ dst-off i) (aget src (+ src-off i)))
               (recur (inc i)))))
  dst)
