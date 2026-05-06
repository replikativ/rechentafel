(ns rechentafel.bench.main
  "JVM entry point for the spread-clj benchmark suite.

  Run:
    clj -M:bench -m rechentafel.bench.main
    clj -M:bench -m rechentafel.bench.main chain wide                 ;; subset
    clj -M:bench -m rechentafel.bench.main --n 50000                  ;; override N

  Prints a one-row summary per shape and exits."
  (:require [rechentafel.bench.runner :as runner]
            [rechentafel.bench.shapes :as shapes]))

(defn- parse-args [args]
  (loop [out {:shapes []} args args]
    (cond
      (empty? args) (update out :shapes (fn [s] (if (seq s) s (vec (keys shapes/all)))))
      (= "--n" (first args)) (recur (assoc out :n (Long/parseLong (second args)))
                                    (drop 2 args))
      (= "--runs" (first args)) (recur (assoc out :runs (Long/parseLong (second args)))
                                       (drop 2 args))
      :else (recur (update out :shapes conj (keyword (first args)))
                   (rest args)))))

(defn -main [& argv]
  (let [{:keys [shapes n runs]} (parse-args argv)
        ns-map (when n (zipmap shapes (repeat n)))]
    (runner/run-all (cond-> {:shapes shapes}
                      n     (assoc :ns ns-map)
                      runs  (assoc :runs runs)))
    (shutdown-agents)))
