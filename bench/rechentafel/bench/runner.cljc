(ns rechentafel.bench.runner
  "Cross-runtime timing harness. Same code measures spread-clj recalc
  performance on JVM and ClojureScript (Node).

  Timing strategy (matching common Clojure/JS bench conventions):
    1. Warm up: run the thunk W times to let the JIT settle.
    2. Measure: collect R samples; report min, median.
    3. Force GC where the runtime allows.

  Timer resolution: System/nanoTime on JVM (nanosecond), js/performance.now
  on cljs (ms with sub-ms precision)."
  (:require [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.bench.shapes :as shapes]
            [rechentafel.functions.all]))

;; ---------------------------------------------------------------------------
;; Cross-runtime timer

(defn- now-ns []
  #?(:clj  (System/nanoTime)
     :cljs (* 1e6 (.now (.-performance js/globalThis)))))

(defn time-ms [thunk]
  (let [t0 (now-ns)
        _  (thunk)
        t1 (now-ns)]
    (/ (- t1 t0) 1e6)))

(defn- gc! []
  #?(:clj  (do (System/gc) (Thread/sleep 50))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Statistics

(defn- median [xs]
  (let [v (vec (sort xs))
        n (count v)]
    (if (zero? n) 0.0
        (nth v (quot n 2)))))

(defn- p95 [xs]
  (let [v (vec (sort xs))
        n (count v)]
    (if (zero? n) 0.0
        (nth v (min (dec n) (long (* 0.95 n)))))))

(defn- stats [xs]
  (let [v (vec xs)]
    {:samples (count v)
     :min     (apply min v)
     :median  (median v)
     :p95     (p95 v)
     :max     (apply max v)}))

;; ---------------------------------------------------------------------------
;; Bench drivers

(defn bench-thunk
  "Run `thunk` `warmup` times to settle, then `runs` times collecting
  per-iteration ms. Returns the stats map."
  [{:keys [warmup runs] :or {warmup 3 runs 7}} thunk]
  (dotimes [_ warmup] (thunk))
  (gc!)
  (stats (for [_ (range runs)] (time-ms thunk))))

(defn- result-of [wb shape n]
  (let [[s r col] (shapes/check-cell shape n)]
    (-> (e/get-cell wb (c/pack s r col)) :v)))

(defn- close? [^double a ^double b]
  ;; loose tolerance — Fact(20) and similar can drift in the last bit
  (< (Math/abs (- a b)) 1e-6))

(defn- check-result [wb shape n]
  (let [got (result-of wb shape n)
        exp (shapes/expected shape n)]
    (cond
      (nil? exp) {:result got :expected :none :match? true}
      (and (number? got) (number? exp) (close? got exp))
      {:result got :expected exp :match? true}
      :else
      {:result got :expected exp :match? false})))

(defn- force-dirty-all
  "Mark every formula dirty so the next `recalc` call has real work
  to do. rechentafel.eval/recalc is incremental: on a clean wb it returns
  immediately, which would zero out the timing."
  [wb]
  (update wb :dirty into (keys (:formulas wb))))

(defn bench-shape
  "Build the workbook for `shape`/`n` once (outside the timed window),
  then time a full `recalc` repeatedly (every formula re-marked dirty
  per iteration). Verifies the result matches the analytical expected
  value for the shape (sets `:match?` false if not). Returns
  `{:shape :n :build-ms :result :expected :match? ...stats}`."
  [shape n & {:as opts}]
  (let [build-ms (time-ms (fn [] (shapes/build shape n)))
        wb       (e/recalc (shapes/build shape n))
        check    (check-result wb shape n)
        st       (bench-thunk opts (fn [] (e/recalc (force-dirty-all wb))))]
    (merge {:shape shape :n n :build-ms build-ms} check st)))

(defn bench-mutate
  "Time the full set-cell + recalc cycle when the cell at A1 mutates
  (e.g. on a star shape, forces transitive dirty propagation through
  every consumer)."
  [shape n & {:as opts}]
  (let [wb0 (e/recalc (shapes/build shape n))]
    (bench-thunk opts
                 (fn []
                   (-> wb0
                       (e/set-cell (c/pack 0 0 0) (rand-int 1000))
                       e/recalc)))))

;; ---------------------------------------------------------------------------
;; Reporting

(defn- runtime-tag []
  #?(:clj  (str "JVM "
                (System/getProperty "java.version"))
     :cljs (str "Node "
                (.-version js/process))))

(defn fmt-row [r]
  (let [tag (cond (false? (:match? r)) (str " MISMATCH got=" (:result r)
                                            " expected=" (:expected r))
                  (= :none (:expected r)) (str " result=" (:result r))
                  :else                   (str " = " (:result r)))]
    #?(:clj  (format "  %-13s n=%-7d  build=%7.2f ms  recalc min=%7.2f  med=%7.2f  p95=%7.2f%s"
                     (name (:shape r)) (long (:n r))
                     (double (:build-ms r))
                     (double (:min r)) (double (:median r)) (double (:p95 r))
                     tag)
       :cljs (str "  " (name (:shape r))
                  "  n=" (:n r)
                  "  build=" (.toFixed (:build-ms r) 2) "ms"
                  "  recalc min=" (.toFixed (:min r) 2)
                  "  med=" (.toFixed (:median r) 2)
                  "  p95=" (.toFixed (:p95 r) 2) "ms"
                  tag))))

(defn run-all
  "Run every shape at its default N, print a one-row summary each.
  Returns the rows."
  [& [{:keys [shapes ns warmup runs]
       :or {shapes (keys shapes/all)}}]]
  (println "spread-clj benchmarks —" (runtime-tag))
  (let [opts (cond-> {} warmup (assoc :warmup warmup) runs (assoc :runs runs))]
    (vec
     (for [s shapes]
       (let [[_ default-n _] (get shapes/all s)
             n (get (or ns {}) s default-n)
             r (bench-shape s n opts)]
         (println (fmt-row r))
         r)))))
