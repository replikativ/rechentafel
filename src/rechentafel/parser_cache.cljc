(ns rechentafel.parser-cache
  "Memoise `rechentafel.parser/parse` keyed on the formula source text.

  Why: profiling (.internal/setcell-profile.md) shows parse takes 8-20
  µs and dominates `set-cell` cost on intern-heavy loads. Filling a
  10k-row column down with `=A2+1`, `=A3+1`, ... costs 10k parses,
  yet every formula has the same RC-normalised AST. The shared-AST
  intern catches the dedup on the AST side; this cache catches it on
  the source-text side, before parse runs at all.

  Mutations to the cached AST are forbidden — callers (notably
  `rc/normalize`) treat ASTs as immutable, so the same AST is safe to
  hand out across hits.

  The cache is process-wide (an atom) and bounded: at `CAP` entries
  we drop to half. This is a coarse LRU but fine in practice — real
  workbooks have hundreds to a few thousand unique formula strings,
  far below the cap.

  Cross-platform: pure Clojure data, works identically on JVM and cljs."
  (:require [rechentafel.parser :as parser]))

(def ^:const CAP 8192)

(defonce ^:private *cache (atom {}))

(defn ^:private evict [m]
  ;; Drop to half capacity. We don't track per-entry recency; this is
  ;; the simplest "throw away half on overflow" policy.
  (if (> (count m) CAP)
    (into {} (take (quot CAP 2) m))
    m))

(defn parse
  "Like `rechentafel.parser/parse` but cached on the source text. The
  returned AST may be shared across calls — never mutate it."
  [^String src]
  (or (get @*cache src)
      (let [ast (parser/parse src)]
        (swap! *cache (fn [m] (evict (assoc m src ast))))
        ast)))

(defn invalidate!
  "Empty the cache. Mostly useful for benchmarking and tests."
  []
  (reset! *cache {}))

(defn stats
  "{:count <entries>}. Handy when probing cache effectiveness."
  []
  {:count (count @*cache)})
