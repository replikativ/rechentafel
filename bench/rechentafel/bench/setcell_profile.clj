(ns rechentafel.bench.setcell-profile
  "Micro-benchmark for the `set-cell` hot path. Builds shapes show
  ~40-100 µs per call; this drills into where the time goes.

  Run: clj -M:bench -m rechentafel.bench.setcell-profile"
  (:require [rechentafel.eval     :as e]
            [rechentafel.cell     :as c]
            [rechentafel.parser   :as parser]
            [rechentafel.rc       :as rc]
            [rechentafel.functions.all]))

(defn- ms [t0 t1] (/ (- (long t1) (long t0)) 1e6))

(defn- bench [name k thunk]
  ;; warmup
  (dotimes [_ 3] (dotimes [_ k] (thunk)))
  (System/gc) (Thread/sleep 50)
  (let [samples (vec (for [_ (range 5)]
                       (let [t0 (System/nanoTime)]
                         (dotimes [_ k] (thunk))
                         (ms t0 (System/nanoTime)))))
        sorted  (vec (sort samples))
        median  (nth sorted 2)
        per-op  (* 1000.0 (/ median k))]
    (println (format "  %-46s  %7.2f µs/op  (k=%d, total=%.1f ms)"
                     name per-op k median))
    {:name name :median-ms median :k k :per-op-µs per-op}))

(defn run! [& _]
  (println "spread-clj set-cell profiling — JVM" (System/getProperty "java.version"))
  (println)
  (println "*** Sub-step costs (synthetic — measure each step in isolation)")

  ;; Parser alone
  (bench "parse `=A1+1`" 100000
         (fn [] (parser/parse "A1+1")))

  (bench "parse `=SUM(A1:A100)`" 100000
         (fn [] (parser/parse "SUM(A1:A100)")))

  (bench "parse `=LET(x, A1, x*2+x)`" 50000
         (fn [] (parser/parse "LET(x, A1, x*2+x)")))

  ;; rc normalize
  (let [ast (parser/parse "A1+1")]
    (bench "rc/normalize `=A1+1`" 100000
           (fn [] (rc/normalize ast 0 0))))

  (println)
  (println "*** End-to-end set-cell — different cell kinds")
  (println "    (each iteration builds a fresh 10k-cell workbook from empty)")

  (let [bench-build
        (fn [name builder]
          (dotimes [_ 3] (builder))                  ;; warmup
          (System/gc) (Thread/sleep 50)
          (let [samples (vec (for [_ (range 5)]
                               (let [t0 (System/nanoTime)]
                                 (builder)
                                 (ms t0 (System/nanoTime)))))
                m (nth (vec (sort samples)) 2)]
            (println (format "  %-46s  %7.2f µs/cell  (10k cells, total=%.1f ms)"
                             name (* 1000.0 (/ m 10000.0)) m))))]

    (bench-build "numeric literals across 10k fresh cells"
                 #(reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 0) (double i)))
                          (e/empty-workbook) (range 10000)))

    (bench-build "string literals across 10k fresh cells"
                 #(reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 0) (str "v" i)))
                          (e/empty-workbook) (range 10000)))

    (bench-build "chain `=A_n+1` (10k different formulas)"
                 #(reduce (fn [wb i]
                            (e/set-cell wb (c/pack 0 i 0)
                                        (if (zero? i) 0 (str "=A" i "+1"))))
                          (e/empty-workbook) (range 10000)))

    (bench-build "repeated `=A1+1` 10k times (intern hit)"
                 #(reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 1) "=A1+1"))
                          (e/empty-workbook) (range 10000)))

    (bench-build "10k × `=SUM(A1:A100)` at fresh cells"
                 #(reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 1) "=SUM(A1:A100)"))
                          (e/empty-workbook) (range 10000))))

  (println)
  (println "*** Recalc — for comparison")
  (let [wb (reduce (fn [wb i]
                     (e/set-cell wb (c/pack 0 i 0)
                                 (if (zero? i) 0 (str "=A" i "+1"))))
                   (e/empty-workbook) (range 10000))]
    (bench "recalc 10k-chain (force-dirty-all per call)" 10
           (fn []
             (e/recalc (update wb :dirty into (keys (:formulas wb)))))))

  (shutdown-agents))

(defn -main [& args] (apply run! args))
