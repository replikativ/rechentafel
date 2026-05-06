(ns rechentafel.fn.datetime
  "Date/time functions (POI category: date/time — 23 fns).

  Excel stores dates as serial numbers: integer part = days since
  1900-01-00 (POI handles the 1904 windowing elsewhere; here we stick
  to the 1900 system that Excel uses by default — including the
  historically-wrong Feb-29-1900 leap day). Times are the fractional
  part: 0.5 = 12:00:00.

  Internally we convert serial → `java.time.LocalDate`/`LocalDateTime`
  on JVM, or the js-joda equivalents on cljs. The arithmetic goes
  through `cljc.java-time`, which mirrors java.time on both runtimes
  (js-joda is a 1700-test ThreeTen port — ISO week, DATEDIF, and
  YEARFRAC semantics match the JVM exactly).

  POI's CalendarFieldFunction uses Calendar.get(...) on a GregorianCalendar
  initialised from the serial. We match with java.time for correctness."
  (:require [clojure.string :as str]
            [cljc.java-time.local-date :as ld]
            [cljc.java-time.local-date-time :as ldt]
            [cljc.java-time.local-time :as lt]
            [cljc.java-time.year :as yr]
            [cljc.java-time.year-month :as ym]
            [cljc.java-time.day-of-week :as dow]
            [cljc.java-time.temporal.chrono-unit :as cu]
            [cljc.java-time.temporal.iso-fields :as iso-fields]
            [cljc.java-time.format.date-time-formatter :as dtf]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; Serial ↔ date conversions
;;
;; Excel 1900 system:
;;   serial 1  = 1900-01-01
;;   serial 60 = 1900-02-29 (doesn't exist — bug preserved for compat)
;;   serial 61 = 1900-03-01
;; For serials >= 61 we can use epoch 1899-12-30 with no adjustment.
;; For serials 1..59 we use epoch 1899-12-31 (skip the phantom leap day).

(def ^:private excel-epoch-normal-str  "1899-12-30")
(def ^:private excel-epoch-pre-leap-str "1899-12-31")

(defn- epoch-normal [] (ld/parse excel-epoch-normal-str))
(defn- epoch-pre-leap [] (ld/parse excel-epoch-pre-leap-str))

(defn- serial->date [serial]
  (let [n (long (Math/floor (double serial)))]
    (if (< n 60)
      (ld/plus-days (epoch-pre-leap) n)
      (ld/plus-days (epoch-normal) n))))

(defn- serial->datetime [serial]
  (let [d   (serial->date serial)
        frac (- (double serial) (Math/floor (double serial)))
        ;; total seconds in the day; round to avoid accumulating error
        secs (Math/round (* frac 86400.0))]
    (-> (ld/at-start-of-day d)
        (ldt/plus-seconds secs))))

(defn- date->serial [d]
  (let [days-n (ld/until (epoch-normal) d cu/days)
        days-p (ld/until (epoch-pre-leap) d cu/days)]
    ;; Use normal epoch for dates on/after 1900-03-01; pre-leap otherwise.
    (if (ld/is-before d (ld/of 1900 3 1))
      (double days-p)
      (double days-n))))

(defn- dt->serial [dt]
  (let [d (ldt/to-local-date dt)
        t (ldt/to-local-time dt)
        day-serial (date->serial d)
        secs (lt/to-second-of-day t)]
    (+ day-serial (/ (double secs) 86400.0))))

;; ---------------------------------------------------------------------------
;; Constructors

(f/register! "DATE"
             (with-meta
               (fn [args]
                 (let [y (long (f/num! (nth args 0)))
                       m (long (f/num! (nth args 1)))
                       d (long (f/num! (nth args 2)))
            ;; Excel: years 0..1899 get +1900; negative years error.
                       y (cond
                           (neg? y) (f/domain-error! :num)
                           (< y 1900) (+ y 1900)
                           :else y)
                       base (ld/of y 1 1)
            ;; month/day overflow rolls forward (Excel behavior)
                       date (-> base
                                (ld/plus-months (dec m))
                                (ld/plus-days (dec d)))]
                   (val/number (date->serial date))))
               {:scalar? true})
             :arity [3 3])

(f/register! "TIME"
             (with-meta
               (fn [args]
                 (let [h (long (f/num! (nth args 0)))
                       m (long (f/num! (nth args 1)))
                       s (long (f/num! (nth args 2)))
                       secs (+ (* h 3600) (* m 60) s)
                       frac (/ (double secs) 86400.0)
            ;; clamp to [0,1) per Excel
                       frac (- frac (Math/floor frac))]
                   (val/number frac)))
               {:scalar? true})
             :arity [3 3])

;; Accept the set of formats POI's DateUtil.parseDateTime handles:
;; ISO, slash-separated US and EU variants, and spelled-out months.
;; We try each pattern in turn; first success wins. Month-name patterns
;; (`MMM`, `MMMM`) require locale data — under cljs the bundled
;; `@js-joda/core` only ships English month abbreviations, which is
;; what we want anyway.
(def ^:private datevalue-patterns
  ["yyyy-M-d"
   "M/d/yyyy"
   "d-MMM-yyyy" "d-MMM-yy"
   "MMM d, yyyy" "MMMM d, yyyy"
   "d MMM yyyy" "d MMMM yyyy"
   "yyyy/M/d"])

(defn- try-parse-date [^String s]
  (or (some (fn [pat]
              (try (ld/parse s (dtf/of-pattern pat))
                   (catch #?(:clj Throwable :cljs :default) _ nil)))
            datevalue-patterns)
      ;; fallback to ISO-local-date iff literal ISO
      (try (ld/parse s) (catch #?(:clj Throwable :cljs :default) _ nil))))

(f/register! "DATEVALUE"
             (with-meta
               (fn [args]
                 (let [s       (f/str! (nth args 0))
                       cleaned (str/trim s)]
                   (if-let [date (try-parse-date cleaned)]
                     (val/number (date->serial date))
                     (f/domain-error! :value))))
               {:scalar? true})
             :arity [1 1])

(f/register! "TIMEVALUE"
             (with-meta
               (fn [args]
                 (let [s (f/str! (nth args 0))
                       t (try (lt/parse s)
                              (catch #?(:clj Throwable :cljs :default) _
                                (f/domain-error! :value)))
                       secs (lt/to-second-of-day t)]
                   (val/number (/ (double secs) 86400.0))))
               {:scalar? true})
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Field extractors — YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY

(defn- field-fn [getter]
  (with-meta
    (fn [args]
      (let [n  (f/num! (nth args 0))
            dt (serial->datetime n)]
        (val/number (double (getter dt)))))
    {:scalar? true}))

(f/register! "YEAR"   (field-fn ldt/get-year)         :arity [1 1])
(f/register! "MONTH"  (field-fn ldt/get-month-value)  :arity [1 1])
(f/register! "DAY"    (field-fn ldt/get-day-of-month) :arity [1 1])
(f/register! "HOUR"   (field-fn ldt/get-hour)         :arity [1 1])
(f/register! "MINUTE" (field-fn ldt/get-minute)       :arity [1 1])
(f/register! "SECOND" (field-fn ldt/get-second)       :arity [1 1])

;; WEEKDAY(serial, [type])
;;   type 1 (default): Sunday=1 ... Saturday=7
;;   type 2: Monday=1 ... Sunday=7
;;   type 3: Monday=0 ... Sunday=6
(f/register! "WEEKDAY"
             (with-meta
               (fn [args]
                 (let [n (f/num! (nth args 0))
                       t (if (> (count args) 1) (long (f/num! (nth args 1))) 1)
                       dow-val (dow/get-value (ld/get-day-of-week (serial->date n))) ;; Mon=1..Sun=7
                       result (case t
                                1 (inc (mod dow-val 7))   ;; Sun=1
                                2 dow-val
                                3 (dec dow-val)
                                (f/domain-error! :num))]
                   (val/number (double result))))
               {:scalar? true})
             :arity [1 2])

;; WEEKNUM(serial, [type]) — simplified: type 1 (default, Sunday-start)
;; and type 2 (Monday-start). Week containing Jan 1 is week 1.
(f/register! "WEEKNUM"
             (with-meta
               (fn [args]
                 (let [n (f/num! (nth args 0))
                       t (if (> (count args) 1) (long (f/num! (nth args 1))) 1)
                       d (serial->date n)
                       year (ld/get-year d)
                       jan1 (ld/of year 1 1)
                       jan1-dow (dow/get-value (ld/get-day-of-week jan1))  ;; Mon=1..Sun=7
            ;; offset so week starts on Sun (t=1) or Mon (t=2)
                       start-offset (case t
                                      1 (mod jan1-dow 7)      ;; days from Sun
                                      2 (dec jan1-dow)
                                      (f/domain-error! :num))
                       day-of-year (ld/get-day-of-year d)]
                   (val/number (double (inc (quot (dec (+ day-of-year start-offset)) 7))))))
               {:scalar? true})
             :arity [1 2])

;; ISOWEEKNUM(serial) — ISO-8601 week number (weeks start Monday; week 1
;; is the one containing the first Thursday of the year).
(f/register! "ISOWEEKNUM"
             (with-meta
               (fn [args]
                 (let [n (f/num! (nth args 0))
                       d (serial->date n)
                       w (ld/get d iso-fields/week-of-week-based-year)]
                   (val/number (double w))))
               {:scalar? true})
             :arity [1 1])

;; ---------------------------------------------------------------------------
;; Volatile: NOW, TODAY

(f/register! "NOW"
             (fn [_args]
               (val/number (dt->serial (ldt/now))))
             :arity [0 0] :volatile? true)

(f/register! "TODAY"
             (fn [_args]
               (val/number (date->serial (ld/now))))
             :arity [0 0] :volatile? true)

;; ---------------------------------------------------------------------------
;; Date arithmetic: DAYS, DAYS360, EDATE, EOMONTH, YEARFRAC, DATEDIF

(f/register! "DAYS"
  ;; DAYS(end_date, start_date) → end - start in days
             (with-meta
               (fn [args]
                 (let [e (f/num! (nth args 0))
                       s (f/num! (nth args 1))]
                   (val/number (double (- (long (Math/floor e)) (long (Math/floor s)))))))
               {:scalar? true})
             :arity [2 2])

(f/register! "DAYS360"
  ;; DAYS360(start, end, [method=FALSE])
  ;; 30-day months, 360-day years.
             (with-meta
               (fn [args]
                 (let [s (serial->date (f/num! (nth args 0)))
                       e (serial->date (f/num! (nth args 1)))
                       method (if (> (count args) 2) (f/bool! (nth args 2)) false)
                       sy (ld/get-year s) sm (ld/get-month-value s) sd (ld/get-day-of-month s)
                       ey (ld/get-year e) em (ld/get-month-value e) ed (ld/get-day-of-month e)
                       [sd ed]
                       (if method
              ;; European (30/360) method: just cap at 30.
                         [(min 30 sd) (min 30 ed)]
              ;; US method: adjustment for end-of-month.
                         (let [sd' (if (= sd 31) 30 sd)
                               ed' (if (and (= ed 31) (>= sd' 30)) 30 ed)]
                           [sd' ed']))]
                   (val/number
                    (double
                     (+ (* 360 (- ey sy))
                        (* 30 (- em sm))
                        (- ed sd))))))
               {:scalar? true})
             :arity [2 3])

(f/register! "EDATE"
  ;; EDATE(start, months) → serial of date N months later.
             (with-meta
               (fn [args]
                 (let [d (serial->date (f/num! (nth args 0)))
                       m (long (f/num! (nth args 1)))]
                   (val/number (date->serial (ld/plus-months d m)))))
               {:scalar? true})
             :arity [2 2])

(f/register! "EOMONTH"
  ;; EOMONTH(start, months) → last day of month N months later.
             (with-meta
               (fn [args]
                 (let [d (serial->date (f/num! (nth args 0)))
                       m (long (f/num! (nth args 1)))
                       target (ld/plus-months d m)
                       eom (ld/with-day-of-month target
                             (ym/length-of-month (ym/from target)))]
                   (val/number (date->serial eom))))
               {:scalar? true})
             :arity [2 2])

(f/register! "DATEDIF"
  ;; DATEDIF(start, end, unit) — unit is "Y","M","D","MD","YM","YD".
             (with-meta
               (fn [args]
                 (let [s (serial->date (f/num! (nth args 0)))
                       e (serial->date (f/num! (nth args 1)))
                       unit (str/upper-case (f/str! (nth args 2)))]
                   (when (ld/is-after s e) (f/domain-error! :num))
                   (val/number
                    (double
                     (case unit
                       "Y"  (ld/until s e cu/years)
                       "M"  (ld/until s e cu/months)
                       "D"  (ld/until s e cu/days)
                       "YM" (mod (ld/until s e cu/months) 12)
                       "YD" (let [se (ld/with-year s (ld/get-year e))
                                  se (if (ld/is-after se e)
                                       (ld/with-year s (dec (ld/get-year e)))
                                       se)]
                              (ld/until se e cu/days))
                       "MD" (let [se (ld/with-month s (ld/get-month-value e))
                                  se (try (ld/with-year se (ld/get-year e))
                                          (catch #?(:clj Throwable :cljs :default) _ se))
                                  se (if (ld/is-after se e)
                                       (ld/minus-months se 1) se)]
                              (ld/until se e cu/days))
                       (f/domain-error! :num))))))
               {:scalar? true})
             :arity [3 3])

(f/register! "YEARFRAC"
  ;; YEARFRAC(start, end, [basis])
  ;; basis: 0=30/360 US (default), 1=actual/actual, 2=actual/360,
  ;;        3=actual/365, 4=30/360 European.
             (with-meta
               (fn [args]
                 (let [sv (f/num! (nth args 0))
                       ev (f/num! (nth args 1))
                       [sv ev] (if (> sv ev) [ev sv] [sv ev])
                       s (serial->date sv)
                       e (serial->date ev)
                       basis (if (> (count args) 2) (long (f/num! (nth args 2))) 0)]
                   (val/number
                    (case basis
                      0 (/ (double
                            (let [sy (ld/get-year s) sm (ld/get-month-value s) sd (ld/get-day-of-month s)
                                  ey (ld/get-year e) em (ld/get-month-value e) ed (ld/get-day-of-month e)
                                  sd' (if (= sd 31) 30 sd)
                                  ed' (if (and (= ed 31) (>= sd' 30)) 30 ed)]
                              (+ (* 360 (- ey sy))
                                 (* 30 (- em sm))
                                 (- ed' sd')))) 360.0)
                      1 (let [days (double (ld/until s e cu/days))
                              y (ld/get-year s)
                              leap? (yr/is-leap (yr/of y))
                              dy (if leap? 366.0 365.0)]
                          (/ days dy))
                      2 (/ (double (ld/until s e cu/days)) 360.0)
                      3 (/ (double (ld/until s e cu/days)) 365.0)
                      4 (/ (double
                            (let [sy (ld/get-year s) sm (ld/get-month-value s) sd (ld/get-day-of-month s)
                                  ey (ld/get-year e) em (ld/get-month-value e) ed (ld/get-day-of-month e)]
                              (+ (* 360 (- ey sy))
                                 (* 30 (- em sm))
                                 (- (min 30 ed) (min 30 sd))))) 360.0)
                      (f/domain-error! :num)))))
               {:scalar? true})
             :arity [2 3])

;; ---------------------------------------------------------------------------
;; NETWORKDAYS, WORKDAY, WORKDAY.INTL

(defn- weekend? [d]
  (let [v (dow/get-value (ld/get-day-of-week d))]
    (or (= v 6) (= v 7))))

(defn- holiday-set [args]
  (let [t (volatile! #{})]
    (f/each-scalar
     args
     (fn [v _]
       (when (val/num? v)
         (vswap! t conj (long (Math/floor (double (:v v))))))))
    @t))

(f/register! "NETWORKDAYS"
             (fn [args]
               (let [sv (f/num! (nth args 0))
                     ev (f/num! (nth args 1))
                     [sv ev sign] (if (> sv ev) [ev sv -1] [sv ev 1])
                     s (serial->date sv)
                     e (serial->date ev)
                     holidays (when (> (count args) 2)
                                (holiday-set [(nth args 2)]))
                     cnt (volatile! 0)]
                 (loop [d s]
                   (when-not (ld/is-after d e)
                     (when (and (not (weekend? d))
                                (not (contains? holidays
                                                (long (Math/floor (date->serial d))))))
                       (vswap! cnt inc))
                     (recur (ld/plus-days d 1))))
                 (val/number (double (* sign @cnt)))))
             :arity [2 3])

(f/register! "WORKDAY"
             (fn [args]
               (let [s (serial->date (f/num! (nth args 0)))
                     days (long (f/num! (nth args 1)))
                     holidays (when (> (count args) 2)
                                (holiday-set [(nth args 2)]))
                     step (if (neg? days) -1 1)
                     target (Math/abs days)
                     skip? (fn [d]
                             (or (weekend? d)
                                 (contains? holidays
                                            (long (Math/floor (date->serial d))))))
                     final (loop [d s, left target]
                             (if (zero? left) d
                                 (let [n (ld/plus-days d step)]
                                   (if (skip? n)
                                     (recur n left)
                                     (recur n (dec left))))))]
                 (val/number (date->serial final))))
             :arity [2 3])

(f/register! "WORKDAY.INTL"
  ;; WORKDAY.INTL(start, days, [weekend-code-or-string], [holidays])
  ;; weekend: "1111100" = Mon-Fri work, Sat/Sun off. Strings are 7 chars, 1=off.
  ;; Default weekend = 1 (Sat+Sun off).
             (fn [args]
               (let [s (serial->date (f/num! (nth args 0)))
                     days (long (f/num! (nth args 1)))
                     wk (when (> (count args) 2) (nth args 2))
                     holidays (when (> (count args) 3) (holiday-set [(nth args 3)]))
                     off-days
                     (cond
                       (nil? wk) #{6 7}
                       (val/str? wk)
                       (let [s (:v wk)]
                         (into #{} (keep-indexed (fn [i c] (when (= c \1) (inc i))) s)))
                       :else
                       (let [code (long (f/num! wk))]
                         (case code
                           1  #{6 7} 2  #{7 1} 3  #{1 2} 4  #{2 3} 5  #{3 4}
                           6  #{4 5} 7  #{5 6}
                           11 #{7} 12 #{1} 13 #{2} 14 #{3} 15 #{4} 16 #{5} 17 #{6}
                           (f/domain-error! :num))))
                     skip? (fn [d]
                             (or (contains? off-days (dow/get-value (ld/get-day-of-week d)))
                                 (contains? holidays (long (Math/floor (date->serial d))))))
                     step (if (neg? days) -1 1)
                     target (Math/abs days)
                     final (loop [d s, left target]
                             (if (zero? left) d
                                 (let [n (ld/plus-days d step)]
                                   (if (skip? n)
                                     (recur n left)
                                     (recur n (dec left))))))]
                 (val/number (date->serial final))))
             :arity [2 4])
