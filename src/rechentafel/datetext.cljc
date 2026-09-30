(ns rechentafel.datetext
  "Text that Excel reads as a date or a time, as a serial number (days since
   1899-12-30, the fraction the time of day): what it coerces in arithmetic
   (\"22:30\"-TIME(0,30,0)), in date functions (EDATE(\"1Oct 21\",1)) and in
   DATEVALUE/TIMEVALUE. en-US readings:

     times         h:mm, h:mm:ss, with AM/PM; alone or after a date
     ISO           yyyy-m-d, yyyy/m/d
     US            m/d/yyyy, m/d/yy, m-d-yyyy; m/d and m-d in the current year
     month names   d MMM yyyy (1 Oct 2021, 1-Oct-21, 1Oct21, 1Jan2024),
                   MMM d, yyyy; MMM yyyy (the 1st); d MMM and MMM d in the
                   current year

   Two-digit years: 00-29 are 2000s, 30-99 1900s. No dependencies: the
   serial is computed from the civil date."
  (:require [clojure.string :as str]))

(def ^:private months
  {"jan" 1 "feb" 2 "mar" 3 "apr" 4 "may" 5 "jun" 6 "jul" 7 "aug" 8 "sep" 9 "sept" 9 "oct" 10 "nov" 11 "dec" 12})

(defn- month-of [w]
  (let [w (str/lower-case w)]
    (or (get months w)
        (when (>= (count w) 3)
          (some (fn [[k m]] (when (and (= 3 (count k)) (str/starts-with? w k)
                                       (str/starts-with? (["january" "february" "march" "april" "may" "june" "july"
                                                           "august" "september" "october" "november" "december"] (dec m))
                                                         w))
                              m))
                months)))))

(defn- days-from-civil
  "Days since 1970-01-01 of y-m-d (proleptic Gregorian; Howard Hinnant)."
  [y m d]
  (let [y (if (<= m 2) (dec y) y)
        era (quot (if (>= y 0) y (- y 399)) 400)
        yoe (- y (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- valid? [y m d]
  (and (<= 1 m 12) (<= 1 d 31) (<= 1900 y 9999)
       (<= d (case m 2 (if (and (zero? (mod y 4)) (or (pos? (mod y 100)) (zero? (mod y 400)))) 29 28)
                   (4 6 9 11) 30 31))))

(defn date-serial
  "The serial of y-m-d (Excel's, from 1900-03-01 on), or nil if invalid."
  [y m d]
  (when (valid? y m d)
    (double (+ (days-from-civil y m d) 25569))))

(defn- this-year []
  #?(:clj (.getYear (java.time.LocalDate/now)) :cljs (.getFullYear (js/Date.))))

(defn- full-year [y]
  (cond (>= y 100) y (< y 30) (+ 2000 y) :else (+ 1900 y)))

(defn- parse-long* [s] #?(:clj (Long/parseLong s) :cljs (js/parseInt s 10)))

(defn- time-part
  "`[fraction rest]` for a time at the end of `s`, or nil."
  [s]
  (when-let [[whole h mi sec ampm] (re-find #"(?i)(\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?\s*(am|pm|a|p)?\s*$" s)]
    (let [h (parse-long* h) mi (parse-long* mi)
          sec #?(:clj (if sec (Double/parseDouble sec) 0.0) :cljs (if sec (js/parseFloat sec) 0.0))
          ampm (some-> ampm str/lower-case first)
          h (case ampm \a (if (= 12 h) 0 h) \p (if (= 12 h) 12 (+ h 12)) h)]
      (when (and (< h 24) (< mi 60) (< sec 60) (or (nil? ampm) (<= 1 (long (mod (+ h 11) 12)) 12)))
        [(/ (+ (* h 3600) (* mi 60) sec) 86400.0) (str/trim (subs s 0 (- (count s) (count whole))))]))))

(defn- date-part
  "The serial of a date `s` (no time), or nil."
  [s]
  (let [t (str/trim s)
        num-tok #"\d+"]
    (or
     ;; yyyy-m-d, yyyy/m/d
     (when-let [[_ y m d] (re-matches #"(\d{4})[-/](\d{1,2})[-/](\d{1,2})" t)]
       (date-serial (parse-long* y) (parse-long* m) (parse-long* d)))
     ;; m/d/yyyy, m-d-yy (US)
     (when-let [[_ m d y] (re-matches #"(\d{1,2})[-/](\d{1,2})[-/](\d{2,4})" t)]
       (date-serial (full-year (parse-long* y)) (parse-long* m) (parse-long* d)))
     ;; m/d, m-d: the current year
     (when-let [[_ m d] (re-matches #"(\d{1,2})[-/](\d{1,2})" t)]
       (date-serial (this-year) (parse-long* m) (parse-long* d)))
     ;; with a month name: tokens of digits and letters
     (let [toks (re-seq #"\d+|[A-Za-z]+" t)
           rest-ok? (re-matches #"[\dA-Za-z\s,.\-/]+" t)
           kind (mapv #(if (re-matches num-tok %) :n (if (month-of %) :m :w)) toks)]
       (when (and rest-ok? (not-any? #{:w} kind))
         (let [n #(parse-long* %)]
           (case kind
             [:n :m :n] (date-serial (full-year (n (nth toks 2))) (month-of (nth toks 1)) (n (nth toks 0)))
             [:m :n :n] (date-serial (full-year (n (nth toks 2))) (month-of (nth toks 0)) (n (nth toks 1)))
             [:n :m] (date-serial (this-year) (month-of (nth toks 1)) (n (nth toks 0)))
             [:m :n] (let [x (n (nth toks 1))]
                       (if (or (> x 31) (= 4 (count (nth toks 1))))
                         (date-serial (full-year x) (month-of (nth toks 0)) 1)
                         (date-serial (this-year) (month-of (nth toks 0)) x)))
             nil)))))))

(defn parse
  "The serial number text `s` means as a date and/or time, or nil."
  [s]
  (when (string? s)
    (let [t (str/trim s)]
      (when (seq t)
        (if-let [[frac before] (time-part t)]
          (if (str/blank? before)
            frac
            (when-let [d (date-part before)] (+ d frac)))
          (date-part t))))))
