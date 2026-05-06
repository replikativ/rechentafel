(ns rechentafel.bench.cljs-main
  "Cljs entry point for the benchmark suite. Run via:

     npx shadow-cljs compile node-bench
     node out/node-bench.js [shape1 shape2 ...] [--n N] [--runs R]

  Same shapes / runner as the JVM build — output format diverges
  slightly because cljs has no `format`."
  (:require [rechentafel.bench.runner :as runner]
            [rechentafel.bench.shapes :as shapes]))

(defn- parse-args [argv]
  (loop [out {:shapes []} args (vec argv)]
    (cond
      (empty? args)
      (update out :shapes (fn [s] (if (seq s) s (vec (keys shapes/all)))))

      (= "--n" (first args))
      (recur (assoc out :n (js/parseInt (second args) 10)) (drop 2 args))

      (= "--runs" (first args))
      (recur (assoc out :runs (js/parseInt (second args) 10)) (drop 2 args))

      :else
      (recur (update out :shapes conj (keyword (first args))) (rest args)))))

(defn main [& argv]
  (let [{:keys [shapes n runs]} (parse-args argv)
        ns-map (when n (zipmap shapes (repeat n)))]
    (runner/run-all (cond-> {:shapes shapes}
                      n     (assoc :ns ns-map)
                      runs  (assoc :runs runs)))))
