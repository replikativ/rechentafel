(ns rechentafel.bench.mtv
  "Compare PHM vs MTV on a realistic workbook shape: 10 000 rows × 20 cols
  with column-homogeneous types (15 numeric + 5 string). Measures bulk
  column-major load, column-scan aggregate (SUM-like), random reads,
  random single-cell writes, and fork cost.

  Bench pattern mirrors stratum: full warmup, probe, then fixed iters,
  report min / median. Uses `System.gc` + sleep between stores."
  (:require [rechentafel.mtv       :as m]
            [rechentafel.workbook  :as wb]
            [rechentafel.cell      :as cell]
            [rechentafel.value     :as val]))

(def ROWS 10000)
(def COLS 20)
(def NUM-COLS 15)       ;; 0..14 numeric
(def STR-COLS  5)       ;; 15..19 string
(def N (* ROWS COLS))

(def NUM-DATA
  (let [a (double-array ROWS)]
    (dotimes [i ROWS] (aset a i (double i)))
    a))

(defn- mk-str-col []
  (let [a (object-array ROWS)]
    (dotimes [i ROWS] (aset a i (val/string (str "s" i))))
    a))

;; Pre-materialize columns so bench measures storage, not data generation.
(def STR-DATA (mk-str-col))

;; Random cell-id + col+row arrays for read bench
(def READ-IDS
  (let [a (long-array 100000)
        r (java.util.Random. 7)]
    (dotimes [i (alength a)]
      (let [row (.nextInt r ROWS)
            col (.nextInt r COLS)]
        (aset a i (cell/pack 0 row col))))
    a))

;; ---------------------------------------------------------------------------
;; Builders

(defn- build-phm []
  (let [s (wb/thm-store)
        ^doubles nd NUM-DATA
        ^objects sd STR-DATA]
    (dotimes [c NUM-COLS]
      (dotimes [r ROWS]
        (wb/-put! s (cell/pack 0 r c) (val/number (aget nd r)))))
    (dotimes [c STR-COLS]
      (let [col (+ NUM-COLS c)]
        (dotimes [r ROWS]
          (wb/-put! s (cell/pack 0 r col) (aget sd r)))))
    (wb/-finish s)))

(defn- build-mtv []
  (loop [c 0 sh (m/empty-sheet)]
    (cond
      (< c NUM-COLS)         (recur (inc c) (m/sheet-put-num-col sh c 0 NUM-DATA))
      (< c (+ NUM-COLS STR-COLS))
      (recur (inc c) (m/sheet-put-gen-col sh c 0 STR-DATA))
      :else sh)))

;; ---------------------------------------------------------------------------
;; Workloads

(defn- phm-scan-col [store ^long col]
  ;; Sum numeric column using the key-oriented store.
  (loop [r 0 acc 0.0]
    (if (= r ROWS) acc
        (let [v (wb/-get store (cell/pack 0 r col))]
          (recur (inc r) (+ acc (if (= :num (:t v)) (double (:v v)) 0.0)))))))

(defn- mtv-scan-col [sheet ^long col]
  (m/col-reduce-num (m/sheet-get-col sheet col) + 0.0))

(defn- phm-random-read-sum [store]
  (let [^longs ids READ-IDS
        n (alength ids)]
    (loop [i 0 acc 0.0]
      (if (= i n) acc
          (let [v (wb/-get store (aget ids i))]
            (recur (inc i) (+ acc (if (= :num (:t v)) (double (:v v)) 0.0))))))))

(defn- mtv-random-read-sum [sheet]
  (let [^longs ids READ-IDS
        n (alength ids)]
    (loop [i 0 acc 0.0]
      (if (= i n) acc
          (let [id (aget ids i)
                row (cell/row id)
                col (cell/col id)
                v (m/sheet-get sheet row col)]
            (recur (inc i) (+ acc (if (= :num (:t v)) (double (:v v)) 0.0))))))))

;; ---------------------------------------------------------------------------
;; Bench helper — stratum-style

(defn- gc! [] (System/gc) (System/gc) (Thread/sleep 50))

(defn- bench [label f & {:keys [warmup iters] :or {warmup 3 iters 7}}]
  (gc!)
  (dotimes [_ warmup] (f))
  (let [ts (vec (for [_ (range iters)]
                  (let [t0 (System/nanoTime)] (f) (- (System/nanoTime) t0))))
        sorted (vec (sort ts))]
    {:label label
     :min   (first sorted)
     :med   (nth sorted (quot iters 2))
     :p90   (nth sorted (min (dec iters) (long (* 0.9 iters))))}))

(defn- fmt-ns [ns]
  (cond (< ns 1000)       (format "%7d ns" ns)
        (< ns 1000000)    (format "%7.1f µs" (double (/ ns 1e3)))
        (< ns 1000000000) (format "%7.1f ms" (double (/ ns 1e6)))
        :else             (format "%7.2f  s" (double (/ ns 1e9)))))

(defn- print-row [{:keys [label min med p90]}]
  (println (format "   %-40s  min=%s  med=%s  p90=%s"
                   label (fmt-ns min) (fmt-ns med) (fmt-ns p90))))

(defn- heap-used []
  (let [rt (Runtime/getRuntime)] (- (.totalMemory rt) (.freeMemory rt))))

(defn- heap-of [build]
  (gc!)
  (let [before (heap-used)
        kept (build)
        _    (gc!)
        after (heap-used)
        mb (double (/ (max 0 (- after before)) 1024.0 1024.0))]
    (identity kept)
    mb))

;; ---------------------------------------------------------------------------
;; Report

(defn report! []
  (println)
  (println "=== MTV vs PHM bench on realistic shape ===")
  (println (format "    %d rows × %d cols = %d cells (15 num + 5 str)" ROWS COLS N))
  (println)
  (let [phm  (build-phm)
        mtv  (build-mtv)]
    (println " bulk build (column-major fill)")
    (print-row (bench "phm  (transient + per-cell put)" build-phm))
    (print-row (bench "mtv  (one block per column)"    build-mtv))
    (println)
    (println " column scan (sum col 0 down 10k rows)")
    (print-row (bench "phm  (10k key lookups)" #(phm-scan-col phm 0)))
    (print-row (bench "mtv  (primitive array reduce)" #(mtv-scan-col mtv 0)))
    (println)
    (println " random reads (100k shuffled cell ids)")
    (print-row (bench "phm" #(phm-random-read-sum phm)))
    (print-row (bench "mtv" #(mtv-random-read-sum mtv)))
    (println)
    (println " heap footprint")
    (println (format "   %-40s  %6.1f MB" "phm" (heap-of build-phm)))
    (println (format "   %-40s  %6.1f MB" "mtv" (heap-of build-mtv)))
    (println)))
