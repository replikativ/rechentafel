(ns rechentafel.fn.misc
  "Miscellaneous functions (POI category: miscellaneous — 49 fns).

  Most entries in this category are Excel-4 macro-language relics
  (ABSREF, CALL, ENABLE.TOOL, GET.CELL, GET.DOCUMENT, GOTO, PRESS.TOOL,
  RETURN, SAVE.TOOLBAR, WINDOW.TITLE, etc.) or server-bound cube
  functions (CUBE*, RTD) that are meaningless in a spreadsheet engine
  without a host application. We register them as #N/A stubs so
  formulas that reference them parse cleanly.

  The genuinely useful ones we implement fully:

    - ADDRESS      — build an A1-style cell reference string
    - DOLLARDE     — fractional dollar → decimal
    - DOLLARFR     — decimal dollar → fractional
    - FVSCHEDULE   — future value under variable interest rates

  Functions that logically belong here but are registered elsewhere
  (SUMIF/SUMIFS in stats, MULTINOMIAL/SERIESSUM/SUBTOTAL/RAND in math)
  are not duplicated."
  (:require [clojure.string :as str]
            [rechentafel.value :as val]
            [rechentafel.functions :as f]))

;; ---------------------------------------------------------------------------
;; ADDRESS

(defn- col-letters
  "0-indexed column → A1 letters (0 → A, 25 → Z, 26 → AA, ...)."
  [^long c]
  (loop [n (inc c) acc ""]
    (if (<= n 0) acc
        (let [rem (mod (dec n) 26)]
          (recur (quot (dec n) 26)
                 (str (char (+ 65 rem)) acc))))))

(f/register! "ADDRESS"
  ;; ADDRESS(row, col, [abs_num], [a1], [sheet_text])
  ;; abs_num: 1=$A$1, 2=A$1, 3=$A1, 4=A1 (default 1)
  ;; a1: true (A1 style, default) or false (R1C1 style)
             ^{:scalar? true}
             (fn [args]
               (let [row (long (f/num! (nth args 0)))
                     col (long (f/num! (nth args 1)))
                     abs-num (if (> (count args) 2) (long (f/num! (nth args 2))) 1)
                     a1?   (if (> (count args) 3) (f/bool! (nth args 3)) true)
                     sheet (when (> (count args) 4) (f/str! (nth args 4)))]
                 (when (or (< row 1) (< col 1)) (f/domain-error! :value))
                 (when-not (<= 1 abs-num 4) (f/domain-error! :value))
                 (let [body (if a1?
                              (let [col-str (col-letters (dec col))]
                                (case abs-num
                                  1 (str "$" col-str "$" row)
                                  2 (str col-str "$" row)
                                  3 (str "$" col-str row)
                                  4 (str col-str row)))
                              (case abs-num
                                1 (str "R" row "C" col)
                                2 (str "R" row "C[" col "]")
                                3 (str "R[" row "]C" col)
                                4 (str "R[" row "]C[" col "]")))]
                   (val/string (if sheet (str sheet "!" body) body)))))
             :arity [2 5])

;; ---------------------------------------------------------------------------
;; DOLLARDE / DOLLARFR

(f/register! "DOLLARDE"
  ;; DOLLARDE(fractional_dollar, fraction) — fraction is the denominator
  ;; DOLLARDE(1.02, 16) means 1 + 2/16 = 1.125
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     frac (f/num! (nth args 1))]
                 (when (< frac 1.0) (f/domain-error! :num))
                 (when (and (zero? frac) (zero? x)) (f/domain-error! :div0))
                 (let [frac (long (Math/floor frac))
                       whole (Math/floor x)
                       rest  (- x whole)
            ;; pow10 of the fraction's # of digits
                       pow (Math/pow 10.0 (Math/ceil (Math/log10 (double frac))))]
                   (val/number (+ whole (/ (* rest pow) (double frac)))))))
             :arity [2 2])

(f/register! "DOLLARFR"
  ;; DOLLARFR(decimal_dollar, fraction) — inverse of DOLLARDE
             ^{:scalar? true}
             (fn [args]
               (let [x (f/num! (nth args 0))
                     frac (f/num! (nth args 1))]
                 (when (< frac 1.0) (f/domain-error! :num))
                 (let [frac (long (Math/floor frac))
                       whole (Math/floor x)
                       rest  (- x whole)
                       pow (Math/pow 10.0 (Math/ceil (Math/log10 (double frac))))]
                   (val/number (+ whole (/ (* rest frac) pow))))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; FVSCHEDULE

(f/register! "FVSCHEDULE"
  ;; Future value of 1 unit compounded by a sequence of rates:
  ;; FVSCHEDULE(principal, {r1, r2, ...}) = principal * Π (1 + r_i)
             (fn [args]
               (let [principal (f/num! (nth args 0))
                     rates (f/collect-finite-numerics [(nth args 1)])]
                 (val/number (reduce * (double principal)
                                     (map #(+ 1.0 (double %)) rates)))))
             :arity [2 2])

;; ---------------------------------------------------------------------------
;; Stubs for Excel-4 macro and cube functions. Returning #N/A here
;; matches POI's NotImplementedFunction behaviour: the formula still
;; parses and evaluates, but the result surfaces as an error rather
;; than a silent zero.

(defn- na-stub [fname arity]
  (f/register! fname (fn [_args] val/ERR-NA) :arity arity))

(doseq [[nm arity] [["ABSREF"        [2 2]]
                    ["APP.TITLE"     [0 1]]
                    ["ARGUMENT"      [0 3]]
                    ["BAHTTEXT"      [1 1]]
                    ["CALL"          [1 3]]
                    ["CUBEKPIMEMBER" [2 4]]
                    ["CUBEMEMBER"    [2 3]]
                    ["CUBEMEMBERPROPERTY" [3 3]]
                    ["CUBERANKEDMEMBER"   [3 4]]
                    ["CUBESET"       [2 5]]
                    ["CUBESETCOUNT"  [1 1]]
                    ["CUBEVALUE"     [1 nil]]
                    ["DATESTRING"    [1 1]]
                    ["ENABLE.TOOL"   [3 3]]
                    ["END.IF"        [0 0]]
                    ["EVALUATE"      [1 1]]
                    ["EXEC"          [1 4]]
                    ["GET.CELL"      [1 2]]
                    ["GET.DOCUMENT"  [1 2]]
                    ["GET.WINDOW"    [1 2]]
                    ["GET.WORKBOOK"  [1 2]]
                    ["GET.WORKSPACE" [1 1]]
                    ["GOTO"          [1 1]]
                    ["LAST.ERROR"    [0 0]]
                    ["NUMBERSTRING"  [2 2]]
                    ["ODDFPRICE"     [8 9]]
                    ["ODDFYIELD"     [8 9]]
                    ["ODDLPRICE"     [7 8]]
                    ["ODDLYIELD"     [7 8]]
                    ["PHONETIC"      [1 1]]
                    ["PRESS.TOOL"    [3 3]]
                    ["REGISTER.ID"   [2 3]]
                    ["RELREF"        [2 2]]
                    ["RETURN"        [1 1]]
                    ["RTD"           [3 nil]]
                    ["SAVE.TOOLBAR"  [0 2]]
                    ["STEP"          [0 0]]
                    ["USDOLLAR"      [1 2]]
                    ["WINDOW.TITLE"  [0 1]]]]
  (na-stub nm arity))
