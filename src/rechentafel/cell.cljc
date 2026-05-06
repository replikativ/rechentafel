(ns rechentafel.cell
  "Cell-id encoding. A cell reference packs into a 50-bit integer:

     [sheet:16 | row:20 | col:14]

  covering Excel's limits — 16 384 columns, 1 048 576 rows, plus 65 536
  sheets for us. Integer keys hash and compare cheaply.

  On JVM we use bit-shift / bit-and on primitive longs (fast). Under
  ClojureScript bitops are 32-bit and would silently truncate, so the
  cljs branch falls back to multiplication / quot / mod within JS's
  53-bit safe-integer range. Same numeric results, slightly slower
  than bitops on the JS path.")

(def ^:const COL-BITS 14)
(def ^:const ROW-BITS 20)
(def ^:const SHEET-BITS 16)

(def ^:const COL-MASK (dec (bit-shift-left 1 COL-BITS)))
(def ^:const ROW-MASK (dec (bit-shift-left 1 ROW-BITS)))
(def ^:const SHEET-MASK (dec (bit-shift-left 1 SHEET-BITS)))

(def ^:const ROW-SHIFT COL-BITS)
(def ^:const SHEET-SHIFT (+ ROW-BITS COL-BITS))

(def ^:const ^:private COL-W   16384)     ;; 2^14
(def ^:const ^:private ROW-W   1048576)   ;; 2^20
(def ^:const ^:private SHEET-W 65536)     ;; 2^16

(defn pack ^long [^long sheet ^long row ^long col]
  #?(:clj  (bit-or (bit-shift-left sheet SHEET-SHIFT)
                   (bit-shift-left row  ROW-SHIFT)
                   col)
     :cljs (+ (* sheet COL-W ROW-W)
              (* row COL-W)
              col)))

(defn sheet ^long [^long id]
  #?(:clj  (bit-and (unsigned-bit-shift-right id SHEET-SHIFT) SHEET-MASK)
     :cljs (mod (Math/floor (/ id (* COL-W ROW-W))) SHEET-W)))

(defn row ^long [^long id]
  #?(:clj  (bit-and (unsigned-bit-shift-right id ROW-SHIFT) ROW-MASK)
     :cljs (mod (Math/floor (/ id COL-W)) ROW-W)))

(defn col ^long [^long id]
  #?(:clj  (bit-and id COL-MASK)
     :cljs (mod id COL-W)))

(defn ->a1 [^long id]
  (let [c (col id)
        letters (loop [n (inc c) acc ""]
                  (if (<= n 0) acc
                      (let [r (mod (dec n) 26)]
                        (recur (quot (dec n) 26)
                               (str (char (+ 65 r)) acc)))))]
    (str letters (inc (row id)))))
