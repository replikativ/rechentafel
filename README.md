# rechentafel

[![CircleCI](https://circleci.com/gh/replikativ/rechentafel.svg?style=shield)](https://circleci.com/gh/replikativ/rechentafel)
[![Clojars](https://img.shields.io/clojars/v/org.replikativ/rechentafel.svg)](https://clojars.org/org.replikativ/rechentafel)

A pure-Clojure spreadsheet interpreter. Parses Excel formulas, builds a
per-cell dependency graph, and evaluates workbooks — no Apache POI, no
LibreOffice, no native deps at runtime.

Works in Clojure and ClojureScript (`.cljc` throughout except for the
optional POI loader).

## Install

```clojure
;; deps.edn
{:deps
 {org.replikativ/rechentafel {:mvn/version "0.1.X"}}}    ;; latest on Clojars
```

Runtime deps:

- `org.clojure/clojure 1.12+`
- `com.widdindustries/cljc.java-time` (used by the datetime functions on
  both runtimes — backed by `java.time` on the JVM and `@js-joda/core`
  on cljs).

POI / XLSX I/O is optional — see [Loading .xlsx files](#loading-xlsx-files).

## Quick start

```clojure
(require '[rechentafel.eval :as e]
         '[rechentafel.cell :as c]
         'rechentafel.functions.all)   ;; registers the built-in function pack

(def wb (-> (e/empty-workbook ["Sheet1"])
            (e/set-cell (c/pack 0 0 0) 10)           ;; A1 = 10
            (e/set-cell (c/pack 0 0 1) 20)           ;; B1 = 20
            (e/set-cell (c/pack 0 0 2) "=A1+B1")     ;; C1 = A1+B1
            (e/recalc)))

(e/get-cell wb (c/pack 0 0 2))
;;=> {:t :num :v 30.0}
```

Cell addresses are packed into a `long` via `(c/pack sheet row col)` —
all 0-indexed. Values come back as tagged maps (`{:t :num/:str/:bool/:err :v ...}`).

Updates are incremental: `set-cell` marks the cell and its transitive
dependents dirty; `recalc` topologically re-evaluates only the dirty set
and writes results back into the sheet store.

## Loading .xlsx files

The interpreter doesn't parse XLSX on its own. The optional
`rechentafel.poi` namespace is a ~80-line wrapper around Apache POI that
replays each cell into a spread workbook:

```clojure
;; deps.edn: add -A:poi or include POI directly
(require '[rechentafel.poi :as poi])
(def wb (poi/load-workbook "model.xlsx"))
;; cells are parsed and recalculated by our interpreter — POI's cached
;; values are ignored.
```

POI is on the classpath only under the `:poi` (or `:dev`) alias, so the
library itself stays pure.

## Architecture

```
                  set-cell / edits
                        │
                        ▼
 parser.cljc  ──► ast (rc-normalized)
                        │
                        ▼
 eval.cljc    ──► dep graph (per-cell reads / volatile / dirty sets)
                        │
                        ▼
 mtv.cljc     ──► multi-typed column sheet store
                        │
                        ▼
 functions/   ──► pluggable registry (math, stats, text, lookup,
   all.cljc         financial, datetime, logical, database,
                    engineering, info, misc)
```

Key modules:

| ns | role |
|---|---|
| `rechentafel.lexer` | Token stream from a formula string |
| `rechentafel.parser` | Pratt parser → AST |
| `rechentafel.rc` | R1C1 ↔ A1 reference normalization |
| `rechentafel.cell` | Pack `(sheet, row, col) → long` |
| `rechentafel.mtv` | Multi-typed vector sheet backing |
| `rechentafel.workbook` | Sheet container + cell read/write |
| `rechentafel.eval` | Dep graph, dirty tracking, topo sort, recalc |
| `rechentafel.functions` | Function registry (`register!`, `call`) |
| `rechentafel.fn.*` | Function implementations (math, stats, …) |
| `rechentafel.value` | Tagged-value constructors + error codes |

## Function coverage

Roughly 270 functions registered by `rechentafel.functions.all`:

- **Math / trig** — arithmetic, logs, powers, full trig + hyperbolic +
  inverse trig, reciprocal trig (COT/SEC/CSC + hyperbolic + inverse), PHI, GAUSS, rounding family
- **Stats** — mean/median/mode, variance/stdev, NORM/T/F/CHI/BINOM/POISSON
  distributions + inverses, rank, percentile, correlation, regression
- **Text** — LEN/FIND/SEARCH/LEFT/RIGHT/MID/SUBSTITUTE/REPLACE/TEXTJOIN,
  TEXT/VALUE/NUMBERVALUE, UNICODE/UNICHAR, TEXTBEFORE/TEXTAFTER
- **Lookup** — VLOOKUP/HLOOKUP/XLOOKUP/XMATCH/INDEX/MATCH/OFFSET/INDIRECT/
  CHOOSE
- **Logical** — IF/IFS/SWITCH/AND/OR/NOT/XOR/IFERROR/IFNA
- **Datetime** — DATE/TIME/YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY/
  ISOWEEKNUM/EDATE/EOMONTH/DAYS/DATEDIF/DATEVALUE/NETWORKDAYS/YEARFRAC
- **Financial** — PV/FV/PMT/NPER/RATE/NPV/IRR/XIRR/XNPV/CUMIPMT/CUMPRINC/
  SLN/SYD/DB/EFFECT/NOMINAL/ISPMT
- **Engineering** — bit ops (AND/OR/XOR/LSHIFT/RSHIFT), BIN2DEC family,
  CONVERT, ERF, BESSEL, complex numbers
- **Info** — CELL/INFO/SHEET/SHEETS/ISFORMULA + full IS* predicates
- **Aggregates** — SUMIF/COUNTIF/AVERAGEIF + multi-criterion variants,
  SUMPRODUCT, SUBTOTAL, AGGREGATE
- **Matrix** — MMULT/MINVERSE/TRANSPOSE/SUMXMY2
- **Database** — DSUM/DCOUNT/DAVERAGE/DMAX/DMIN/...

## Running tests

```bash
bin/run-unittests             # pure JVM tests (clojure -M:test)
bin/run-cljstests             # cljs tests under Node (needs `npm install` first)
clj -M:poi-oracle             # POI parity oracle (opt-in, pulls POI deps)
```

The oracle suite builds each formula under test *both* in our engine and
in POI, then diffs results — cross-validates Excel semantics.

## Benchmarks

```bash
clj -M:bench -m rechentafel.bench.main            # all shapes, JVM
npx shadow-cljs release node-bench && \
  node out/node-bench.js                          # all shapes, cljs

clj -M:bench -m rechentafel.bench.compare         # vs LibreOffice + POI
```

Numbers and methodology in `.internal/bench-results.md`. On 10k-cell
shapes the engine is in the same ballpark as Apache POI and ~70× faster
than LibreOffice headless.

## Status

Excel 365 feature coverage including LET / LAMBDA / dynamic arrays /
3D refs / structured table refs / FORMULATEXT. Cross-runtime correctness
verified against LibreOffice and POI on the benchmark fixtures
(`test/rechentafel/golden_test.cljc`).
