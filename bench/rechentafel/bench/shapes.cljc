(ns rechentafel.bench.shapes
  "Workbook shapes used by the benchmarks. Each fn returns a populated
  spread-clj workbook (un-recalculated). Pure cljc so the same fixtures
  run on JVM and cljs.

  Shapes are chosen to stress different parts of the engine:
    :chain        — long dep chain (A1=0, A2=A1+1, …)
                    stresses topo-sort + serial eval
    :wide         — N independent formulas (B_i = A_i*2)
                    stresses parallel-able recalc loop overhead
    :aggregate    — one big SUM(A1:AN)
                    stresses range materialisation + walk-scalars
    :star         — N formulas reading the same anchor
                    stresses transitive-dirty + sector-rdep
    :spill        — A1 = SEQUENCE(N), single anchor fills N rows
                    stresses spill materialisation + sibling writes
    :lambda-rec   — recursive factorial via named LAMBDA
                    stresses LAMBDA call overhead + env extension"
  (:require [rechentafel.eval :as e]
            [rechentafel.cell :as c]))

(defn chain
  "A1 = 0, A_i = A_{i-1}+1 for i in 1..N-1."
  [^long n]
  (reduce (fn [wb i]
            (e/set-cell wb (c/pack 0 i 0)
                        (if (zero? i) 0 (str "=A" i "+1"))))
          (e/empty-workbook) (range n)))

(defn wide
  "A1..AN = literal integers; B1..BN = `=A_i * 2`."
  [^long n]
  (let [wb0 (e/empty-workbook)
        wb1 (reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 0) (double i)))
                    wb0 (range n))]
    (reduce (fn [wb i]
              (e/set-cell wb (c/pack 0 i 1)
                          (str "=A" (inc i) "*2")))
            wb1 (range n))))

(defn aggregate
  "A1..AN = literal integers; B1 = SUM(A1:AN)."
  [^long n]
  (let [wb0 (e/empty-workbook)
        wb1 (reduce (fn [wb i] (e/set-cell wb (c/pack 0 i 0) (double i)))
                    wb0 (range n))]
    (e/set-cell wb1 (c/pack 0 0 1) (str "=SUM(A1:A" n ")"))))

(defn star
  "A1 = 100; B1..BN = `=A1 * i`."
  [^long n]
  (let [wb0 (e/set-cell (e/empty-workbook) (c/pack 0 0 0) 100)]
    (reduce (fn [wb i]
              (e/set-cell wb (c/pack 0 i 1)
                          (str "=A1*" (inc i))))
            wb0 (range n))))

(defn spill
  "A1 = SEQUENCE(N) — single anchor materialising N siblings."
  [^long n]
  (-> (e/empty-workbook)
      (e/set-cell (c/pack 0 0 0) (str "=SEQUENCE(" n ")"))))

(defn lambda-rec
  "Recursive factorial via a named LAMBDA. `n` is the factorial input,
  not a workbook size — the cell at A1 returns Fact(n)."
  [^long n]
  (-> (e/empty-workbook)
      (e/define-name "Fact" "=LAMBDA(k, IF(k<=1, 1, k*Fact(k-1)))")
      (e/set-cell (c/pack 0 0 0) (str "=Fact(" n ")"))))

(def ^:const all
  "Registry of every shape. Keyed by name; values are
  `[builder default-n description]`."
  {:chain      [chain      10000  "Linear chain A_i = A_{i-1}+1"]
   :wide       [wide       10000  "Independent formulas B_i = A_i*2"]
   :aggregate  [aggregate  10000  "B1 = SUM(A1:AN)"]
   :star       [star       10000  "B_i = A1*i (single anchor, N readers)"]
   :spill      [spill      10000  "A1 = SEQUENCE(N) — spill materialisation"]
   :lambda-rec [lambda-rec 100    "Recursive factorial"]})

(defn build
  "Build a workbook for the given shape + size. Returns un-recalculated."
  [shape n]
  (let [[builder _default-n _] (get all shape)]
    (when-not builder (throw (ex-info "unknown shape" {:shape shape})))
    (builder n)))

;; ---------------------------------------------------------------------------
;; Result verification — every shape has one cell whose final value we
;; can predict analytically, so we can compare answers across runtimes
;; (JVM vs cljs) and across engines (vs POI / LibreOffice).

(defn check-cell
  "Coordinates of the cell whose value should match across runtimes
  and engines for the given shape. Returns `[sheet row col]`."
  [shape n]
  (case shape
    :chain      [0 (dec n)   0]      ;; A_N
    :wide       [0 (dec n)   1]      ;; B_N (=A_N * 2)
    :aggregate  [0 0         1]      ;; B1 = SUM(A1:AN)
    :star       [0 (dec n)   1]      ;; B_N (=A1 * N)
    :spill      [0 (dec n)   0]      ;; A_N
    :lambda-rec [0 0         0]))    ;; A1 = Fact(N)

(defn expected
  "Analytical expected value for the result cell. Returns nil for
  shapes whose closed form is awkward (lambda-rec) — those just use
  the JVM result as the cross-check baseline."
  [shape n]
  (case shape
    :chain      (double (dec n))
    :wide       (double (* 2 (dec n)))
    :aggregate  (double (/ (* (long n) (dec (long n))) 2))
    :star       (double (* 100 n))
    :spill      (double n)
    :lambda-rec nil))
