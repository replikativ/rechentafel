(ns rechentafel.bench.compare
  "Comparative wall-clock benchmarks against external engines.

  Built around the shapes in `rechentafel.bench.shapes`. For each shape we:

    1. Build the same workbook in spread-clj's data model.
    2. Serialise to .xlsx via Apache POI.
    3. Time three engines on the same .xlsx:
         - LibreOffice headless (load → recalc → save)
         - POI's FormulaEvaluator (load → evaluateAll)
         - rechentafel.poi/load-workbook + rechentafel/recalc

  POI's FormulaEvaluator gives the apples-to-apples in-memory baseline
  for a JVM Excel engine; LibreOffice is the apples-to-oranges
  commercial-quality reference (its number includes process startup).

  IronCalc and Excel itself are not measured here — IronCalc would need
  a Rust harness and we don't have Excel on this machine.

  Run:
    clj -M:bench -m rechentafel.bench.compare                    ;; default shapes
    clj -M:bench -m rechentafel.bench.compare chain aggregate    ;; subset
    clj -M:bench -m rechentafel.bench.compare --n 5000           ;; smaller N"
  (:require [clojure.java.shell :as sh]
            [rechentafel.bench.runner :as runner]
            [rechentafel.bench.shapes :as shapes]
            [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.poi :as poi-loader]
            [rechentafel.poi-writer :as poi-writer]
            [rechentafel.functions.all])
  (:import [java.io File]
           [org.apache.poi.ss.usermodel WorkbookFactory FormulaEvaluator]
           [java.io FileInputStream]))

;; ---------------------------------------------------------------------------
;; XLSX fixtures — write the spread-clj workbook out to a real .xlsx so
;; we can hand it to LibreOffice / POI.

(defn- write-xlsx! [wb ^String path]
  ;; reuse our writer; it round-trips through POI's XSSF
  (poi-writer/save-workbook wb path)
  path)

(defn- temp-xlsx [shape n]
  (let [f (doto (File/createTempFile (str "spread-bench-" (name shape) "-")
                                     ".xlsx")
            (.deleteOnExit))
        wb (e/recalc (shapes/build shape n))]
    (write-xlsx! wb (.getAbsolutePath f))
    f))

;; ---------------------------------------------------------------------------
;; Engines

(defn- ms ^double [t0 t1] (/ (- (long t1) (long t0)) 1e6))

(defn- bench-thunk [warmup runs thunk]
  (dotimes [_ warmup] (thunk))
  (System/gc) (Thread/sleep 50)
  (let [samples (vec (for [_ (range runs)]
                       (let [t0 (System/nanoTime)]
                         (thunk)
                         (ms t0 (System/nanoTime)))))
        sorted  (vec (sort samples))]
    {:min    (first sorted)
     :median (nth sorted (quot (count sorted) 2))
     :p95    (nth sorted (min (dec (count sorted)) (long (* 0.95 (count sorted)))))}))

(defn- read-cell-value
  "Read the cached numeric value of [sheet, row, col] from a saved
  .xlsx via POI. Used to extract results from files written by
  LibreOffice, our writer, or POI itself."
  [^File xlsx [^long sh ^long row ^long col]]
  (with-open [in (FileInputStream. xlsx)
              wb (WorkbookFactory/create in)]
    (let [s (.getSheetAt ^org.apache.poi.ss.usermodel.Workbook wb sh)
          r (.getRow s row)
          c (when r (.getCell r col))]
      (when c
        (case (str (.getCachedFormulaResultType c))
          "NUMERIC" (.getNumericCellValue c)
          (try (.getNumericCellValue c) (catch Throwable _ nil)))))))

(def ^:private home (System/getProperty "user.home"))

(defn- libreoffice-recalc-ms
  "Runs `libreoffice --headless --convert-to xlsx` end-to-end. Returns
  `{:median ms :result <number from saved xlsx> :saved-path file}`.

  Snap-packaged soffice can't read /tmp, so we copy the input XLSX and
  the output dir into $HOME first."
  [^File xlsx check-coords]
  (let [in-copy  (doto (File. home "spread-bench-in.xlsx") (.delete))
        out-dir  (doto (File. home "spread-bench-lo-out") (.mkdirs))
        _        (clojure.java.io/copy xlsx in-copy)
        saved-name (.getName in-copy)
        saved   (atom nil)
        timing  (bench-thunk
                 1 3
                 (fn []
                   (.delete (File. out-dir saved-name))
                   (let [{:keys [exit err]}
                         (sh/sh "libreoffice" "--headless" "--calc"
                                "--convert-to" "xlsx"
                                "--outdir" (.getAbsolutePath out-dir)
                                (.getAbsolutePath in-copy))]
                     (when (not= 0 exit)
                       (throw (ex-info "libreoffice failed" {:err err})))
                     (let [s (File. out-dir saved-name)]
                       (when (.exists s) (reset! saved s))))))
        result  (when @saved (read-cell-value @saved check-coords))]
    (assoc timing :result result :saved-path @saved)))

(defn- poi-evaluate-all-ms
  "Loads the .xlsx fresh into POI per iteration and runs
  FormulaEvaluator.evaluateAll(), then reads the value at
  `check-coords`. Returns `{:median ms :result <number>}`."
  [^File xlsx [^long sh ^long row ^long col :as check-coords]]
  (let [last-result (atom nil)
        timing (bench-thunk
                2 5
                (fn []
                  (with-open [in  (FileInputStream. xlsx)
                              wb  (WorkbookFactory/create in)]
                    (let [ev (.. wb getCreationHelper createFormulaEvaluator)]
                      (.evaluateAll ev)
                      (let [s (.getSheetAt ^org.apache.poi.ss.usermodel.Workbook wb sh)
                            r (.getRow s row)
                            c (when r (.getCell r col))]
                        (reset! last-result
                                (when c (try (.getNumericCellValue c)
                                             (catch Throwable _ nil)))))))))]
    (assoc timing :result @last-result)))

(defn- spread-load-recalc-ms
  "Spread-clj's full path: load .xlsx via the POI loader (which copies
  cells into our engine) then recalc. Slightly different from
  runner/bench-shape because that builds in-memory; here we measure
  the load step too."
  [^File xlsx]
  (bench-thunk
   2 5
   (fn []
     (let [wb (poi-loader/load-workbook (.getAbsolutePath xlsx))]
       (e/recalc wb)))))

(defn- force-dirty-all
  "Mark every formula in the workbook dirty so the next recalc has
  real work to do. Needed because rechentafel.eval/recalc is incremental:
  on an already-clean wb it returns immediately."
  [wb]
  (update wb :dirty into (keys (:formulas wb))))

(defn- spread-recalc-only-ms
  "Apples-to-apples with FormulaEvaluator: build the workbook once,
  time only repeated full recalc passes (force every formula dirty
  on each iteration). Records the result at check-coords."
  [shape n check-coords]
  (let [wb     (e/recalc (shapes/build shape n))
        result (-> wb (e/get-cell (apply c/pack check-coords)) :v)
        timing (bench-thunk
                2 5
                (fn [] (e/recalc (force-dirty-all wb))))]
    (assoc timing :result result)))

;; ---------------------------------------------------------------------------
;; Driver

(defn- fmt [^double x]
  (cond
    (>= x 10000) (format "%6.0f" x)
    (>= x 100)   (format "%6.1f" x)
    :else        (format "%6.2f" x)))

(defn- print-row [shape n results expected]
  (let [cell  (fn [r k] (if r (fmt (k r)) "   ---"))
        results-eq? (fn [r]
                      (let [v (:result r)]
                        (cond
                          (nil? v) :missing
                          (nil? expected) :unknown
                          (and (number? v)
                               (< (Math/abs (- (double v) (double expected))) 1e-6))
                          :match
                          :else :mismatch)))
        flag (fn [r] (case (results-eq? r) :match "✓" :mismatch "✗"
                           :missing "?" :unknown "·"))]
    (println
     (format "  %-12s n=%-7d  LO=%s ms %s  POI=%s ms %s  spread=%s ms %s   (load=%s ms)"
             (name shape) (long n)
             (cell (:lo results) :median)            (flag (:lo results))
             (cell (:poi results) :median)           (flag (:poi results))
             (cell (:spread-recalc results) :median) (flag (:spread-recalc results))
             (cell (:spread-load results) :median)))))

(defn run-shape! [shape n {:keys [skip-lo? skip-poi?]}]
  (let [coords  (shapes/check-cell shape n)
        expected (shapes/expected shape n)
        xlsx (temp-xlsx shape n)
        lo  (when-not skip-lo? (libreoffice-recalc-ms xlsx coords))
        poi (when-not skip-poi?
              (try (poi-evaluate-all-ms xlsx coords)
                   (catch Throwable e
                     (println "  POI eval failed:" (.getMessage e))
                     nil)))
        sl  (try (spread-load-recalc-ms xlsx)
                 (catch Throwable e
                   (println "  spread load failed:" (.getMessage e))
                   nil))
        sr  (spread-recalc-only-ms shape n coords)]
    (print-row shape n {:lo lo :poi poi :spread-load sl :spread-recalc sr} expected)
    {:shape shape :n n :expected expected
     :lo lo :poi poi :spread-load sl :spread-recalc sr}))

(defn -main [& argv]
  (let [{:keys [shapes n skip-lo skip-poi runs]
         :or   {shapes (filter #(not= :lambda-rec %) (keys shapes/all))}}
        (loop [out {:shapes []} args (vec argv)]
          (cond
            (empty? args) (update out :shapes (fn [s] (if (seq s) s nil)))
            (= "--skip-lo" (first args))  (recur (assoc out :skip-lo true) (rest args))
            (= "--skip-poi" (first args)) (recur (assoc out :skip-poi true) (rest args))
            (= "--n" (first args))    (recur (assoc out :n (Long/parseLong (second args)))
                                             (drop 2 args))
            (= "--runs" (first args)) (recur (assoc out :runs (Long/parseLong (second args)))
                                             (drop 2 args))
            :else (recur (update out :shapes conj (keyword (first args))) (rest args))))
        shapes (or shapes (filter #(not= :lambda-rec %) (keys shapes/all)))]
    (println "Compare spread-clj against LibreOffice + POI on shared .xlsx fixtures")
    (println "(LO timing includes process startup; POI/spread are warm in-process)")
    (doseq [s shapes]
      (let [[_ default-n _] (get shapes/all s)
            n (or n default-n)]
        (run-shape! s n {:skip-lo? skip-lo :skip-poi? skip-poi})))
    (shutdown-agents)))
