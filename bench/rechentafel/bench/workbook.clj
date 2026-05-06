(ns rechentafel.bench.workbook
  "Micro-benchmarks for value-store backings. Run from the REPL:

     (require '[rechentafel.bench.workbook :as b] :reload)
     (b/report!)

  Measures wall time (ns) for: bulk write, random read, and sequential read
  on 100k cells. Persistent map, transient-backed map, and a primitive
  column store are compared."
  (:require [rechentafel.workbook :as wb]
            [rechentafel.value    :as val]
            [rechentafel.cell     :as cell]))

(defn- elapsed [thunk]
  ;; warmup
  (dotimes [_ 2] (thunk))
  (let [runs 5
        ts   (vec (for [_ (range runs)]
                    (let [t0 (System/nanoTime)]
                      (thunk)
                      (- (System/nanoTime) t0))))]
    {:min-ns (apply min ts)
     :median (nth (vec (sort ts)) (quot runs 2))
     :mean-ns (long (/ (reduce + ts) runs))}))

(defn- bytes->mb [b] (double (/ b 1024 1024)))

(defn- heap-used []
  (let [rt (Runtime/getRuntime)]
    (- (.totalMemory rt) (.freeMemory rt))))

(defn- gc! []
  (System/gc) (System/gc) (Thread/sleep 50))

(defn- heap-of [build]
  (gc!)
  (let [before (heap-used)
        _kept  (build)
        _      (gc!)
        after  (heap-used)]
    (identity _kept)
    (bytes->mb (max 0 (- after before)))))

;; ---------------------------------------------------------------------------
;; Workload

(def N 100000)
(def WIDTH 256)       ;; cols
(def HEIGHT 400)      ;; rows — 256×400 = 102400 slots, >= N

(defn- rand-ids [^long n]
  (let [ids (long-array n)]
    (dotimes [i n]
      (let [r (long (/ i WIDTH))
            c (rem i WIDTH)]
        (aset ids i (cell/pack 0 r c))))
    ids))

(def ^:private IDS (rand-ids N))
(def ^:private VALS
  (let [a (double-array N)]
    (dotimes [i N] (aset a i (Math/random)))
    a))

;; ---------------------------------------------------------------------------
;; Write benchmarks

(defn- bulk-phm []
  (loop [i 0 s (wb/phm-store)]
    (if (= i N) s
        (recur (inc i)
               (wb/-put s (aget ^longs IDS i)
                        (val/number (aget ^doubles VALS i)))))))

(defn- bulk-thm []
  (let [s (wb/thm-store)]
    (dotimes [i N]
      (wb/-put! s (aget ^longs IDS i)
                (val/number (aget ^doubles VALS i))))
    (wb/-finish s)))

(defn- bulk-cols []
  (let [s (wb/cols-store WIDTH HEIGHT)]
    (dotimes [i N]
      (wb/-put! s (aget ^longs IDS i)
                (val/number (aget ^doubles VALS i))))
    s))

;; ---------------------------------------------------------------------------
;; Read benchmarks

(defn- seq-read-sum [store]
  (let [n (alength ^longs IDS)]
    (loop [i 0 acc 0.0]
      (if (= i n) acc
          (let [v (wb/-get store (aget ^longs IDS i))]
            (recur (inc i) (+ acc (double (:v v)))))))))

(defn- rand-read-sum [store ^longs shuffled]
  (let [n (alength shuffled)]
    (loop [i 0 acc 0.0]
      (if (= i n) acc
          (let [v (wb/-get store (aget shuffled i))]
            (recur (inc i) (+ acc (double (:v v)))))))))

(defn- shuffled-ids []
  (let [n N
        a (long-array n)
        r (java.util.Random. 42)]
    (System/arraycopy IDS 0 a 0 n)
    (dotimes [i n]
      (let [j (.nextInt r (- n i))
            tmp (aget a i)]
        (aset a i (aget a (+ i j)))
        (aset a (+ i j) tmp)))
    a))

;; ---------------------------------------------------------------------------
;; Report

(defn- fmt-ns [ns]
  (cond
    (< ns 1000)       (format "%7d ns" ns)
    (< ns 1000000)    (format "%7.1f µs" (double (/ ns 1e3)))
    (< ns 1000000000) (format "%7.1f ms" (double (/ ns 1e6)))
    :else             (format "%7.2f  s" (double (/ ns 1e9)))))

(defn report! []
  (println)
  (println "=== workbook value-store bench  (" N "cells, single sheet) ===")
  (println)
  ;; Precompute stores once for read bench
  (let [phm     (bulk-phm)
        _       (println "building stores...")
        cols    (bulk-cols)
        shufids (shuffled-ids)]
    (println)
    (println " writes (build the store)")
    (doseq [[label f] [["phm  (persistent assoc in loop)" bulk-phm]
                       ["thm  (transient! + persistent!)" bulk-thm]
                       ["cols (primitive column arrays)"  bulk-cols]]]
      (let [{:keys [min-ns mean-ns]} (elapsed f)]
        (println (format "   %-34s  min=%s  mean=%s"
                         label (fmt-ns min-ns) (fmt-ns mean-ns)))))
    (println)
    (println " sequential reads (iterate + sum :v)")
    (doseq [[label store] [["phm"  phm]
                           ["cols" cols]]]
      (let [{:keys [min-ns mean-ns]}
            (elapsed #(seq-read-sum store))]
        (println (format "   %-34s  min=%s  mean=%s"
                         label (fmt-ns min-ns) (fmt-ns mean-ns)))))
    (println)
    (println " random reads (shuffled ids)")
    (doseq [[label store] [["phm"  phm]
                           ["cols" cols]]]
      (let [{:keys [min-ns mean-ns]}
            (elapsed #(rand-read-sum store shufids))]
        (println (format "   %-34s  min=%s  mean=%s"
                         label (fmt-ns min-ns) (fmt-ns mean-ns)))))
    (println)
    (println " fork + 1k extra writes (copy-on-write cost)")
    (doseq [[label mk mut] [["phm  (structural share)"
                             #(wb/-forked phm)
                             (fn [s]
                               (loop [i 0 s s]
                                 (if (= i 1000) s
                                     (recur (inc i)
                                            (wb/-put s (aget ^longs IDS i)
                                                     (val/number 0.0))))))]
                            ["cols (array copy)"
                             #(wb/-forked cols)
                             (fn [s]
                               (dotimes [i 1000]
                                 (wb/-put! s (aget ^longs IDS i)
                                           (val/number 0.0)))
                               s)]]]
      (let [{:keys [min-ns mean-ns]}
            (elapsed #(mut (mk)))]
        (println (format "   %-34s  min=%s  mean=%s"
                         label (fmt-ns min-ns) (fmt-ns mean-ns)))))
    (println)
    (println " heap footprint (estimated from Runtime)")
    (println (format "   %-34s  %6.1f MB"
                     "phm"  (heap-of #(bulk-phm))))
    (println (format "   %-34s  %6.1f MB"
                     "cols" (heap-of #(bulk-cols))))
    (println)))
