(ns rechentafel.bench.libreoffice-baseline
  "Wall-time benchmark comparing spread-clj's recalc against LibreOffice
  Calc's `--headless --convert-to` pipeline.

  Build a workbook of known shape (linear chain, grid with SUM, …) and
  write it to an XLSX via POI with formulas pinned. Time LibreOffice's
  headless load-recalc-save cycle; time our in-memory recalc. Return
  both, plus the ratio.

  LibreOffice's time includes process startup + XML parse + write-back
  so it's an end-to-end number, not a pure-compute one. Our recalc is
  pure-compute. Any `1000x` ratio under small N is mostly LO startup —
  the interesting numbers come at N=50k+.

  Run:
    (require '[bench.libreoffice-baseline :as b])
    (b/run! {:shape :chain :n 10000})

  Needs `libreoffice` on PATH."
  (:refer-clojure :exclude [run!])
  (:require [clojure.java.shell   :as sh]
            [rechentafel.eval       :as e]
            [rechentafel.cell       :as c]
            [rechentafel.functions.all])
  (:import [org.apache.poi.xssf.usermodel XSSFWorkbook]
           [java.io FileOutputStream]))

;; ---------------------------------------------------------------------------
;; XLSX writer — build a sheet of N cells where each row is either a
;; literal or a formula the reader must recalc.

(defn- set-formula-cell [^org.apache.poi.ss.usermodel.Row row ^long col ^String formula]
  (-> row (.createCell col org.apache.poi.ss.usermodel.CellType/FORMULA)
      (.setCellFormula formula)))

(defn- set-num-cell [^org.apache.poi.ss.usermodel.Row row ^long col ^double v]
  (-> row (.createCell col org.apache.poi.ss.usermodel.CellType/NUMERIC)
      (.setCellValue v)))

(defn build-chain-xlsx
  "Write N-row XLSX where A1=0, A2=A1+1, A3=A2+1, … — a linear dependency
  chain of length N. Returns the java.io.File."
  [^long n]
  (let [wb    (XSSFWorkbook.)
        sheet (.createSheet wb "S1")
        f     (doto (java.io.File/createTempFile "spread-chain-" ".xlsx")
                (.deleteOnExit))]
    (doseq [i (range n)]
      (let [row (.createRow sheet i)]
        (if (zero? i)
          (set-num-cell row 0 0.0)
          (set-formula-cell row 0 (str "A" i "+1")))))
    (with-open [out (FileOutputStream. f)]
      (.write wb out))
    (.close wb)
    f))

;; ---------------------------------------------------------------------------
;; Timing helpers

(defn- time-ms [thunk]
  (let [t0 (System/nanoTime)]
    (thunk)
    (/ (- (System/nanoTime) t0) 1e6)))

(defn- median [xs]
  (let [s (vec (sort xs))] (nth s (quot (count s) 2))))

(defn- best-of [n thunk]
  (dotimes [_ 1] (thunk))           ;; warmup
  (median (for [_ (range n)] (time-ms thunk))))

;; ---------------------------------------------------------------------------
;; LibreOffice baseline

(defn libreoffice-recalc-ms
  "Time LibreOffice's headless load → recalc → save cycle on `xlsx`.
  End-to-end: includes soffice startup, XML parse, write-back."
  [^java.io.File xlsx]
  (let [out-dir (doto (java.io.File/createTempFile "spread-lo-" "")
                  (.delete) (.mkdirs))]
    (time-ms
     (fn []
       (let [{:keys [exit err]}
             (sh/sh "libreoffice" "--headless" "--calc"
                    "--convert-to" "xlsx"
                    "--outdir" (.getAbsolutePath out-dir)
                    (.getAbsolutePath xlsx))]
         (when (not= 0 exit)
           (throw (ex-info "libreoffice failed" {:err err}))))))))

(def ^:private home (System/getProperty "user.home"))

(defn libreoffice-pure-recalc-ms
  "Pure-recalc measurement inside LibreOffice — excludes process startup.
  Delegates timing to a Basic macro (Standard.Bench.Run) that loops
  calculateAll() inside soffice and writes the median ms to a result file.

  The macro must be installed in the user's LO Basic; see
  bench/README-libreoffice.md. The xlsx is copied into $HOME because the
  snap build can't read /tmp.

  Returns the median of `runs` calculateAll() invocations in ms."
  [^java.io.File xlsx ^long runs]
  (let [copy   (doto (java.io.File. home "bench-input.xlsx") (.delete))
        out    (java.io.File. home "bench-lo-result.txt")
        log    (java.io.File. home "bench-lo-result.txt.log")]
    (clojure.java.io/copy xlsx copy)
    (.delete out) (.delete log)
    (let [macro-url (format "macro:///Standard.Bench.Run(\"%s\",\"%d\")"
                            (.getAbsolutePath out) runs)
          {:keys [exit err]}
          (sh/sh "libreoffice" "--invisible" "--norestore" "--nologo"
                 "--nodefault" "--nofirststartwizard"
                 (.getAbsolutePath copy) macro-url)]
      (when (not= 0 exit)
        (throw (ex-info "libreoffice macro failed"
                        {:err err :log (when (.exists log) (slurp log))})))
      (if (.exists out)
        (Double/parseDouble (.trim (slurp out)))
        (throw (ex-info "no result file" {:log (when (.exists log) (slurp log))}))))))

;; ---------------------------------------------------------------------------
;; spread-clj side

(defn spread-chain-recalc-ms
  "Build a wb in our engine of the same shape, then time recalc. We
  pre-warm by running one recalc before the measurement."
  [^long n]
  (let [wb (reduce (fn [wb i]
                     (e/set-cell wb (c/pack 0 i 0)
                                 (if (zero? i) 0 (str "=A" i "+1"))))
                   (e/empty-workbook) (range n))
        _  (dotimes [_ 2] (e/recalc wb))]  ;; JIT warmup
    (best-of 5 #(e/recalc wb))))

;; ---------------------------------------------------------------------------
;; Public entry point

(defn run!
  "Run the benchmark. `opts` keys:
     :shape   :chain (default)
     :n       length (default 10000)
     :runs    number of LO runs for median (default 3)
     :pure?   when true, measures LO recalc inside soffice (excludes
              startup/IO); requires the Bench.xba macro to be installed."
  [{:keys [shape n runs pure?] :or {shape :chain n 10000 runs 3 pure? false}}]
  (println (format "== shape=%s n=%d ==" shape n))
  (let [xlsx (case shape
               :chain (build-chain-xlsx n))
        lo-ms (if pure?
                (libreoffice-pure-recalc-ms xlsx (max runs 3))
                (best-of runs #(libreoffice-recalc-ms xlsx)))
        spread-ms (spread-chain-recalc-ms n)
        label (if pure? "libreoffice (pure recalc):" "libreoffice (headless recalc+save):")]
    (println (format "%-38s %8.1f ms" label lo-ms))
    (println (format "%-38s %8.1f ms" "spread-clj (recalc only):" spread-ms))
    (println (format "spread-clj is %.2fx %s"
                     (if (>= lo-ms spread-ms) (/ lo-ms spread-ms) (/ spread-ms lo-ms))
                     (if (>= lo-ms spread-ms) "faster" "slower")))
    {:shape shape :n n :libreoffice-ms lo-ms :spread-ms spread-ms :pure? pure?}))
