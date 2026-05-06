(ns rechentafel.address
  "A1-notation ↔ [row col] conversion, and sheet-qualified address parsing.

  Internal representation is zero-indexed [row col] pairs. A1 is the
  presentation format POI exposes to users and that appears in formula
  strings. Matches POI's CellReference semantics without depending on it."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Column letter ↔ index. 'A' = 0, 'Z' = 25, 'AA' = 26, 'XFD' = 16383.

(defn col-letters->idx ^long [^String s]
  (let [s (str/upper-case s)
        n (count s)]
    (loop [i 0, acc 0]
      (if (>= i n)
        (dec acc)
        (let [c #?(:clj  (long (.charAt s i))
                   :cljs (.charCodeAt s i))
              d (- c 64)]
          (recur (inc i) (+ (* acc 26) d)))))))

(defn col-idx->letters [^long idx]
  (loop [n (inc idx), acc ""]
    (if (zero? n)
      acc
      (let [r (mod (dec n) 26)
            c #?(:clj  (char (+ 65 r))
                 :cljs (.fromCharCode js/String (+ 65 r)))]
        (recur (quot (dec n) 26) (str c acc))))))

;; ---------------------------------------------------------------------------
;; A1 ↔ [row col]

(def ^:private a1-re
  "Matches optional $'s, letters, digits."
  #"^(\$?)([A-Za-z]+)(\$?)(\d+)$")

(defn parse-a1
  "Parse an A1 ref into {:row :col :abs-row? :abs-col?}. Returns nil on miss
  or when the row is zero (rows are 1-indexed in A1 notation, so `B0` is
  not a valid cell — POI falls back to defined-name lookup there)."
  [^String s]
  (when-let [[_ abs-col letters abs-row digits] (re-matches a1-re s)]
    (let [row-n #?(:clj  (Long/parseLong digits)
                   :cljs (js/parseInt digits 10))]
      (when (pos? row-n)
        {:col      (col-letters->idx letters)
         :row      (dec row-n)
         :abs-col? (= abs-col "$")
         :abs-row? (= abs-row "$")}))))

(defn format-a1
  "Render [row col] as A1. `abs-row?` / `abs-col?` default to false."
  ([row col] (format-a1 row col false false))
  ([row col abs-row? abs-col?]
   (str (when abs-col? "$")
        (col-idx->letters col)
        (when abs-row? "$")
        (inc row))))

;; ---------------------------------------------------------------------------
;; Sheet-qualified refs: 'Sheet1'!A1, Sheet1!$B$2, 'Income Statement'!A1:B5

(defn parse-sheet-prefix
  "Split 'Sheet'!tail into [sheet tail] or [nil s] when no sheet qualifier.
  Handles single-quoted sheet names with doubled-quote escaping."
  [^String s]
  (cond
    (str/starts-with? s "'")
    (loop [i 1, buf ""]
      (cond
        (>= i (count s)) [nil s]
        (= \' (get s i))
        (if (= \' (get s (inc i) \space))
          (recur (+ i 2) (str buf \'))
          (if (= \! (get s (inc i) \space))
            [buf (subs s (+ i 2))]
            [nil s]))
        :else (recur (inc i) (str buf (get s i)))))

    :else
    (if-let [[_ sheet tail] (re-matches #"([A-Za-z_][A-Za-z0-9_.]*)!(.*)" s)]
      [sheet tail]
      [nil s])))
