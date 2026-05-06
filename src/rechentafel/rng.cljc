(ns rechentafel.rng
  "Seedable PRNG used by RAND / RANDARRAY / RANDBETWEEN.

  `clojure.core/rand` and `js/Math.random` are process-global and
  unseedable, which makes test fixtures and any reproducibility-
  sensitive workflow brittle.

  Design:

    - `wb[:rng-seed]` is an optional long. When present, `recalc`
      binds a thread-local PRNG state seeded from it for the duration
      of that recalc; all RAND-family fns draw from it.
    - When absent, fall back to `clojure.core/rand` so existing code
      keeps working with no changes.
    - The PRNG itself is xorshift64* on JVM (one Java long per state)
      and a 64-bit-emulating xorshift on cljs (using two 32-bit
      halves because JS bitops are 32-bit). Same sequence on both
      runtimes given the same seed.

  Usage:
    (-> wb (assoc :rng-seed 42) e/recalc)   ;; reproducible
    (-> wb (assoc :rng-seed 43) e/recalc)   ;; different sequence
    (e/recalc wb)                           ;; uses Math.random (legacy)

  The state advances within a single recalc and is discarded between
  recalcs — re-binding from the same seed yields the same sequence.")

;; ---------------------------------------------------------------------------
;; xorshift64* — period 2^64 - 1, decent statistical quality, tiny state.
;;
;; JVM implementation uses primitive long math.
;; cljs splits the state into two unsigned 32-bit halves and emulates
;; the shift+xor operations across them. Result: same numeric
;; sequence on both runtimes (modulo the final double conversion,
;; which discards bits below precision).

#?(:clj
   (deftype JvmRng [^:unsynchronized-mutable ^long state]
     clojure.lang.IFn
     (invoke [_]
       ;; xorshift64*. Multiplication and shifts wrap modulo 2^64 by
       ;; design — use unchecked-* on the multiply step (the bit-shift
       ;; ops never overflow).
       (let [x state
             x (bit-xor x (bit-shift-left x 13))
             x (bit-xor x (unsigned-bit-shift-right x 7))
             x (bit-xor x (bit-shift-left x 17))
             x (unchecked-multiply x 0x2545F4914F6CDD1D)]
         (set! state x)
         ;; convert to [0, 1) using top 53 bits
         (* (unsigned-bit-shift-right x 11) (/ 1.0 9.007199254740992E15))))))

#?(:cljs
   (defn- ^number jcljs-step [state]
     ;; cljs xorshift64* using a goog.math.Long-like split. We avoid
     ;; importing goog.math.Long to keep bundle size; instead store
     ;; state as a pair [hi32 lo32] of unsigned 32-bit halves and do
     ;; the shifts manually.
     (let [hi (aget state 0)
           lo (aget state 1)
           ;; x ^= x << 13
           [hi lo] (let [shi (bit-or (bit-shift-left hi 13)
                                     (unsigned-bit-shift-right lo 19))
                         slo (bit-shift-left lo 13)
                         shi (bit-and shi 0xFFFFFFFF)
                         slo (bit-and slo 0xFFFFFFFF)]
                     [(bit-xor hi shi) (bit-xor lo slo)])
           ;; x ^= x >>> 7
           [hi lo] (let [shi (unsigned-bit-shift-right hi 7)
                         slo (bit-or (bit-shift-left hi 25)
                                     (unsigned-bit-shift-right lo 7))
                         slo (bit-and slo 0xFFFFFFFF)]
                     [(bit-xor hi shi) (bit-xor lo slo)])
           ;; x ^= x << 17
           [hi lo] (let [shi (bit-or (bit-shift-left hi 17)
                                     (unsigned-bit-shift-right lo 15))
                         slo (bit-shift-left lo 17)
                         shi (bit-and shi 0xFFFFFFFF)
                         slo (bit-and slo 0xFFFFFFFF)]
                     [(bit-xor hi shi) (bit-xor lo slo)])]
       (aset state 0 hi)
       (aset state 1 lo)
       ;; build a double in [0,1) from the top 53 bits
       (let [hi (unsigned-bit-shift-right hi 0) ;; ensure unsigned
             lo (unsigned-bit-shift-right lo 0)]
         (* (+ (* hi 4294967296.0) lo)
            (/ 1.0 18446744073709552000.0))))))

#?(:cljs
   (deftype CljsRng [state]
     IFn
     (-invoke [_] (jcljs-step state))))

(defn make
  "Build a seeded RNG. Returns an opaque handle; call it as a 0-arg
  function to draw the next double in [0,1)."
  [^long seed]
  ;; 0 is a degenerate state for xorshift; replace with a constant.
  (let [seed (if (zero? seed) 0xdeadbeef seed)]
    #?(:clj (JvmRng. seed)
       :cljs (let [hi (bit-and (unsigned-bit-shift-right seed 32) 0xFFFFFFFF)
                   lo (bit-and seed 0xFFFFFFFF)
                   hi (if (zero? (+ hi lo)) 0xdeadbeef hi)
                   arr #js [hi lo]]
               (CljsRng. arr)))))

;; ---------------------------------------------------------------------------
;; Thread-local binding used by RAND-family fns.

(def ^:dynamic *rng*
  "Bound by `rechentafel.eval/recalc` when wb has `:rng-seed`. Otherwise
  nil; RAND falls back to `clojure.core/rand`."
  nil)

(defn next-double
  "Draw the next double in [0,1) from `*rng*` if bound, else from the
  process-wide `clojure.core/rand`."
  ^double []
  (if-let [r *rng*] (r) (rand)))
