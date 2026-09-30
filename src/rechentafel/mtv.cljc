(ns rechentafel.mtv
  "Multi-type-vector column storage. Each column is a sequence of typed
  blocks covering contiguous row ranges. Adjacent blocks are never the
  same type (except :empty may abut). Dense runs live in primitive
  arrays; empty runs take one block entry regardless of length. Ported
  in spirit from LibreOffice's mdds::multi_type_vector, tuned for the
  tall/column-homogeneous shape of real Excel workbooks.

  Block tags:
    :empty  — no data, just a row range
    :num    — java double[] of :len entries
    :gen    — java Object[] (strings, bools, errors, formulas, ...)

  A workbook sheet is a vector of columns (one per column index), each a
  `Column` record holding its blocks vector. Only touched columns are
  allocated; everything past the last-used column returns BLANK. We use
  persistent vectors for both the blocks vector and the column array so
  fork is O(1) via structural sharing; bulk loads use a transient mode
  that delays coalescing/split work."
  (:require [rechentafel.platform :as p]))

(def BLANK {:t :blank :v nil})

;; ---------------------------------------------------------------------------
;; Block record

;; `data` is never written once a block holds it (every write builds new
;; arrays), so blocks share it: a block is the `len` elements from `off`,
;; and clipping one is a view, not a copy
(defrecord Block [^long start ^long len type data ^long off])

(defn empty-block [^long start ^long len]
  (->Block start len :empty nil 0))

(defn num-block ^Block [^long start data]
  (->Block start (p/arr-len data) :num data 0))

(defn gen-block ^Block [^long start data]
  (->Block start (count data) :gen (p/objects-from data) 0))

;; ---------------------------------------------------------------------------
;; Binary search: find block index covering row, or -1 if past end.

(defn block-idx ^long [blocks ^long row]
  (let [n (count blocks)]
    (loop [lo 0 hi (dec n)]
      (if (> lo hi) -1
          (let [mid (unsigned-bit-shift-right (+ lo hi) 1)
                ^Block b (nth blocks mid)
                s (.-start b)
                e (+ s (.-len b))]
            (cond
              (< row s)  (recur lo (dec mid))
              (>= row e) (recur (inc mid) hi)
              :else      mid))))))

;; ---------------------------------------------------------------------------
;; Read

(defn- block-val [^Block b ^long row]
  (case (.-type b)
    :empty BLANK
    :num   (let [a (.-data b)]
             {:t :num :v (aget a (+ (.-off b) (- row (.-start b))))})
    :gen   (let [a (.-data b)
                 v (aget a (+ (.-off b) (- row (.-start b))))]
             (if (nil? v) BLANK v))))

(defn col-get [col ^long row]
  (let [blocks (:blocks col)
        max-row (:max-row col)]
    (if (or (neg? row) (> row max-row))
      BLANK
      (let [i (block-idx blocks row)]
        (if (neg? i)
          BLANK
          (block-val (nth blocks i) row))))))

;; ---------------------------------------------------------------------------
;; Bulk construction — write a dense array over a row range. Replaces any
;; blocks entirely contained in the range; clips edge blocks.

(defn- tag-of [v]
  (case (:t v)
    :num :num
    :gen))

(defn- clip-block-before ^Block [^Block b ^long cut-row]
  ;; Return the prefix of b that ends strictly before cut-row, or nil.
  (let [e (+ (.-start b) (.-len b))]
    (cond
      (>= (.-start b) cut-row) nil
      (< e cut-row) b
      :else
      (let [new-len (- cut-row (.-start b))]
        (->Block (.-start b) new-len (.-type b) (.-data b) (.-off b))))))

(defn- clip-block-after ^Block [^Block b ^long cut-row]
  ;; Return the suffix of b that starts at cut-row, or nil.
  (let [e (+ (.-start b) (.-len b))]
    (cond
      (>= cut-row e) nil
      (<= cut-row (.-start b)) b
      :else
      (let [offset  (- cut-row (.-start b))
            new-len (- (.-len b) offset)]
        (->Block cut-row new-len (.-type b) (.-data b) (+ (.-off b) offset))))))

(declare col-put-block*)

(defn col-put-block
  "Replace the row range covered by `new-block` with it. Existing blocks
  before or after the range are kept; those overlapping are clipped on
  both edges. No empty-block padding is maintained — reads past the last
  block return BLANK directly."
  [col ^Block new-block]
  (let [blocks  (:blocks col)
        max-row (long (:max-row col))
        ns      (.-start new-block)
        ne      (+ ns (.-len new-block))
        n       (count blocks)
        ^Block last-b (when (pos? n) (nth blocks (dec n)))
        last-e  (if last-b (+ (.-start last-b) (.-len last-b)) 0)]
    (cond
      ;; past every block (a column filled top to bottom, as a loader and
      ;; a recalc's flush write it): adjacent and of the same type, the
      ;; last block grows (one copy); else the block is appended — no scan
      (and last-b (= ns last-e) (= :empty (.-type last-b) (.-type new-block)))
      (assoc col :blocks (assoc blocks (dec n) (->Block (.-start last-b) (+ (.-len last-b) (.-len new-block)) :empty nil 0))
             :max-row (max max-row (dec ne)))

      (and last-b (= ns last-e) (= (.-type last-b) (.-type new-block)) (#{:num :gen} (.-type new-block)))
      (let [la (.-len last-b) lb (.-len new-block)
            dst (if (= :num (.-type new-block)) (p/make-num-array (+ la lb)) (p/make-gen-array (+ la lb)))]
        (p/arr-copy! (.-data last-b) (.-off last-b) dst 0 la)
        (p/arr-copy! (.-data new-block) (.-off new-block) dst la lb)
        (assoc col :blocks (assoc blocks (dec n) (->Block (.-start last-b) (+ la lb) (.-type new-block) dst 0))
               :max-row (max max-row (dec ne))))

      (>= ns last-e)
      (assoc col :blocks (conj blocks new-block) :max-row (max max-row (dec ne)))

      ;; one cell inside a block of its type (overwriting a value): that
      ;; block, copied with the one element replaced
      (and (= 1 (.-len new-block)) (#{:num :gen} (.-type new-block))
           (let [i (block-idx blocks ns)]
             (and (not (neg? i)) (= (.-type ^Block (nth blocks i)) (.-type new-block)))))
      (let [i (block-idx blocks ns)
            ^Block b (nth blocks i)
            len (.-len b)
            dst (if (= :num (.-type b)) (p/make-num-array len) (p/make-gen-array len))]
        (p/arr-copy! (.-data b) (.-off b) dst 0 len)
        (aset dst (- ns (.-start b)) (aget (.-data new-block) (.-off new-block)))
        (assoc col :blocks (assoc blocks i (->Block (.-start b) len (.-type b) dst 0))))

      :else
      (col-put-block* col new-block blocks max-row ns ne n))))

(defn- emit!
  "`out` (a transient vector of blocks) with `b` after them; an empty block
   right after an empty one joins it, so blanking a run cell by cell leaves
   one block."
  [out ^Block b]
  (let [c (count out)
        ^Block l (when (pos? c) (nth out (dec c)))]
    (if (and l (= :empty (.-type l) (.-type b)) (= (+ (.-start l) (.-len l)) (.-start b)))
      (assoc! out (dec c) (->Block (.-start l) (+ (.-len l) (.-len b)) :empty nil 0))
      (conj! out b))))

(defn- col-put-block* [col new-block blocks max-row ns ne n]
  (loop [i 0
         out (transient [])
         inserted? false]
    (if (= i n)
      (let [out (if inserted? out (emit! out new-block))
            new-max (max max-row (dec ne))]
        (assoc col :blocks (persistent! out) :max-row new-max))
      (let [^Block b (nth blocks i)
            bs (.-start b)
            be (+ bs (.-len b))]
        (cond
            ;; b strictly before new-block
          (<= be ns)
          (recur (inc i) (emit! out b) inserted?)

            ;; b strictly after new-block → emit new-block first if not done
          (>= bs ne)
          (let [out (if inserted? out (emit! out new-block))]
            (recur (inc i) (emit! out b) true))

            ;; b overlaps new-block → keep only its non-overlapping edges
          :else
          (let [out (if-let [p (clip-block-before b ns)] (emit! out p) out)
                out (if inserted? out (emit! out new-block))
                out (if-let [s (clip-block-after b ne)]  (emit! out s) out)]
            (recur (inc i) out true)))))))

(defn col-put-num-range
  "Write a num-array over [start-row, start-row + (arr-len data))."
  [col ^long start-row data]
  (col-put-block col (num-block start-row data)))

(defn col-put-gen-range
  "Write a gen-array (or seqable) over the rows starting at start-row."
  [col ^long start-row data]
  (col-put-block col (gen-block start-row data)))

;; Single-cell write: build a 1-element block and splice.
(defn col-put [col ^long row v]
  (case (:t v)
    :num   (let [a (p/make-num-array 1)]
             (aset a 0 (double (:v v)))
             (col-put-num-range col row a))
    :blank (col-put-block col (empty-block row 1))
    (let [a (p/make-gen-array 1)]
      (aset a 0 v)
      (col-put-gen-range col row a))))

;; ---------------------------------------------------------------------------
;; Column constructor

(defn empty-column []
  {:blocks [] :max-row -1})

;; ---------------------------------------------------------------------------
;; Iteration — walk blocks in order, yielding tagged values. Primarily for
;; scan-style aggregates (SUM, AVG) where the evaluator wants direct access
;; to the primitive arrays without per-cell allocation.

(defn col-reduce-num
  "Reduce over numeric cells in a column, skipping empty/generic runs.
  `f` takes (acc ^double x) and returns the new acc. Fast-path for the
  hot aggregate families."
  [col f init]
  (reduce (fn [acc ^Block b]
            (if (= :num (.-type b))
              (let [a (.-data b)
                    o (.-off b)
                    n (+ o (.-len b))]
                (loop [i o acc acc]
                  (if (= i n) acc
                      (recur (inc i) (f acc (aget a i))))))
              acc))
          init
          (:blocks col)))

(defn col-count-num [col]
  (col-reduce-num col (fn [^long acc _] (inc acc)) 0))

;; ---------------------------------------------------------------------------
;; Sheet — a vector of columns (index = column number).

(defn empty-sheet [] [])

(defn sheet-get-col [sheet ^long c]
  (if (< c (count sheet))
    (nth sheet c)
    (empty-column)))

(defn sheet-put-col [sheet ^long c col]
  (cond
    (< c (count sheet))     (assoc sheet c col)
    (= c (count sheet))     (conj sheet col)
    :else                   (into (into sheet (repeat (- c (count sheet)) (empty-column)))
                                  [col])))

(defn sheet-get [sheet ^long r ^long c]
  (col-get (sheet-get-col sheet c) r))

(defn sheet-put [sheet ^long r ^long c v]
  (sheet-put-col sheet c (col-put (sheet-get-col sheet c) r v)))

(defn sheet-put-num-col
  "Bulk-load a numeric column with a num-array of row-aligned values."
  [sheet ^long c ^long start-row data]
  (sheet-put-col sheet c (col-put-num-range (sheet-get-col sheet c) start-row data)))

(defn sheet-put-gen-col
  "Bulk-load a non-numeric column with a gen-array of values."
  [sheet ^long c ^long start-row data]
  (sheet-put-col sheet c (col-put-gen-range (sheet-get-col sheet c) start-row data)))
