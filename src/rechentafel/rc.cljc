(ns rechentafel.rc
  "R1C1 normalization for shared-formula interning.

  A formula's A1-form AST is cell-specific (a2 at B1 and b2 at B2 look
  different). Converting to R1C1 form — where relative refs become
  offsets from the owning cell — lets identical formulas hash equal.

  `normalize` walks an A1 AST and rewrites :ref / :range nodes:
    - absolute refs (abs-row?/abs-col? flagged by the parser) stay as
      literal row/col;
    - relative refs become {:rc? true :drow n :dcol m}.

  `resolve-at` is the inverse — given an origin cell (row,col), re-hydrate
  an R1C1 ref into an A1-form ref with the actual row/col filled in.

  Interning: `hash-of` collapses the AST to a stable hash; callers keep
  `{hash → ast}` and dedupe. The small hit on install time is paid back
  many times over during eval (shared-struct caches, fewer GC roots).")

(defn- normalize-ref [ref origin-row origin-col]
  (let [ar? (boolean (:abs-row? ref))
        ac? (boolean (:abs-col? ref))
        row (long (:row ref 0))
        col (long (:col ref 0))
        ;; Build a minimal RC-form map. We deliberately drop parser
        ;; bookkeeping (:text, :pos, :ws-before?) since it rots the hash
        ;; with positional noise. :last-sheet (3D Sheet1:Sheet3 form)
        ;; and :workbook (external [Book]Sheet form) are part of the
        ;; ref's identity and must be preserved for evaluation to know
        ;; the spans / books later.
        base (cond-> {:op :ref :sheet (:sheet ref) :whole (:whole ref)
                      :abs-row? ar? :abs-col? ac?}
               (:last-sheet ref) (assoc :last-sheet (:last-sheet ref))
               (:workbook   ref) (assoc :workbook   (:workbook ref)))
        base (cond-> base
               ar?        (assoc :row row)
               (not ar?)  (assoc :rc-row? true :drow (- row (long origin-row)))
               ac?        (assoc :col col)
               (not ac?)  (assoc :rc-col? true :dcol (- col (long origin-col))))]
    base))

(defn normalize
  "Walk `ast`, turning every :ref's relative row/col into an offset from
  `origin` (row,col). Returns a fresh AST that hashes identically to
  any other formula with the same R1C1 shape."
  [ast origin-row origin-col]
  (case (:op ast)
    :ref   (normalize-ref ast origin-row origin-col)
    :range (-> ast
               (assoc :left  (normalize-ref (:left ast) origin-row origin-col))
               (assoc :right (normalize-ref (:right ast) origin-row origin-col)))
    :call  (update ast :args (fn [args]
                               (mapv #(normalize % origin-row origin-col) args)))
    :binop (-> ast
               (update :left  normalize origin-row origin-col)
               (update :right normalize origin-row origin-col))
    :unop   (update ast :arg normalize origin-row origin-col)
    :postop (update ast :arg normalize origin-row origin-col)
    :union  (update ast :args (fn [args]
                                (mapv #(normalize % origin-row origin-col) args)))
    :intersect (-> ast
                   (update :left  normalize origin-row origin-col)
                   (update :right normalize origin-row origin-col))
    :array  (update ast :rows
                    (fn [rows]
                      (mapv (fn [row]
                              (mapv #(normalize % origin-row origin-col) row))
                            rows)))
    :let    (-> ast
                (update :bindings
                        (fn [bs]
                          (mapv (fn [[n v]] [n (normalize v origin-row origin-col)]) bs)))
                (update :body normalize origin-row origin-col))
    :single-cell (update ast :arg normalize origin-row origin-col)
    :spill-ref (update ast :anchor normalize-ref origin-row origin-col)
    :lambda (update ast :body normalize origin-row origin-col)
    :lambda-call (-> ast
                     (update :fn normalize origin-row origin-col)
                     (update :args
                             (fn [as] (mapv #(normalize % origin-row origin-col) as))))
    ast))

(defn resolve-ref
  "Hydrate a single :ref from RC to A1 form using `origin`. If the ref
  is already absolute on a dimension, that dimension is untouched."
  [ref origin-row origin-col]
  (let [row (if (:rc-row? ref)
              (+ (long origin-row) (long (:drow ref)))
              (long (:row ref 0)))
        col (if (:rc-col? ref)
              (+ (long origin-col) (long (:dcol ref)))
              (long (:col ref 0)))]
    (-> ref
        (dissoc :rc-row? :rc-col? :drow :dcol)
        (assoc :row row :col col))))

(defn resolve-at
  "Walk an R1C1-form AST and materialise every :ref against `origin`.
  Used by eval + dep-collect so they can keep their A1-form logic."
  [ast origin-row origin-col]
  (case (:op ast)
    :ref   (resolve-ref ast origin-row origin-col)
    :range (-> ast
               (assoc :left  (resolve-ref (:left ast) origin-row origin-col))
               (assoc :right (resolve-ref (:right ast) origin-row origin-col)))
    :call  (update ast :args (fn [args]
                               (mapv #(resolve-at % origin-row origin-col) args)))
    :binop (-> ast
               (update :left  resolve-at origin-row origin-col)
               (update :right resolve-at origin-row origin-col))
    :unop   (update ast :arg resolve-at origin-row origin-col)
    :postop (update ast :arg resolve-at origin-row origin-col)
    :union  (update ast :args (fn [args]
                                (mapv #(resolve-at % origin-row origin-col) args)))
    :intersect (-> ast
                   (update :left  resolve-at origin-row origin-col)
                   (update :right resolve-at origin-row origin-col))
    :array  (update ast :rows
                    (fn [rows]
                      (mapv (fn [row]
                              (mapv #(resolve-at % origin-row origin-col) row))
                            rows)))
    :let    (-> ast
                (update :bindings
                        (fn [bs]
                          (mapv (fn [[n v]] [n (resolve-at v origin-row origin-col)]) bs)))
                (update :body resolve-at origin-row origin-col))
    :single-cell (update ast :arg resolve-at origin-row origin-col)
    :spill-ref (update ast :anchor resolve-ref origin-row origin-col)
    :lambda (update ast :body resolve-at origin-row origin-col)
    :lambda-call (-> ast
                     (update :fn resolve-at origin-row origin-col)
                     (update :args
                             (fn [as] (mapv #(resolve-at % origin-row origin-col) as))))
    ast))

(defn hash-of
  "Stable hash for an R1C1-normalised AST. Plain Clojure hash is already
  structural; wrap it so callers don't accidentally pass non-normalised
  ASTs and get misses."
  ^long [rc-ast]
  (hash rc-ast))
