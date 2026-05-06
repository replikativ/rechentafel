# Changelog

All notable changes to `org.replikativ/rechentafel`.

## [0.1.x] — initial public release

First public release of the rechentafel spreadsheet interpreter.

### Engine
- Pure-Clojure formula parser, AST + canonical unparser (round-trips at
  the AST level)
- Per-cell dependency graph with sector-bucketed reverse-dep index
- Incremental recalc with topological order + cycle detection
- Multi-typed vector (MTV) column store with bulk-load fast path
- Process-wide bounded parser-text cache for repeated formula patterns

### Functions (~270 registered)
- math, text, datetime, financial, statistics, engineering, info, lookup,
  database, misc, array
- Excel 365 dynamic-array set: SEQUENCE, RANDARRAY, MUNIT, TRANSPOSE,
  CHOOSEROWS, CHOOSECOLS, DROP, TAKE, EXPAND, HSTACK, VSTACK, TOROW,
  TOCOL, WRAPROWS, WRAPCOLS, UNIQUE, SORT, SORTBY, FILTER
- LAMBDA helpers: MAP, REDUCE, SCAN, BYROW, BYCOL, MAKEARRAY, ISOMITTED
- Datetime via `cljc.java-time` (java.time on JVM, js-joda on cljs)

### Language features
- LET, LAMBDA, ISOMITTED, named-LAMBDA UDFs (recursive supported)
- Tables (structured refs `Sales[Amount]`, `[#Headers]`/`[#Data]`/`[#Totals]`/
  `[@col]`)
- 3D references `Sheet1:Sheet3!A1:B5`
- FORMULATEXT
- Array broadcasting in binops
- `@` (implicit intersection / SINGLE) and `A1#` (ANCHORARRAY) operators
- Spill on set with `#SPILL!` blocked detection, shape-shrink cleanup,
  dirty-anchor propagation when blockers move

### Cross-platform
- Clojure 1.12+ on JVM
- ClojureScript (Node + browser) via shadow-cljs
- Same `.cljc` source for the entire interpreter

### XLSX I/O (optional, behind `:poi` alias)
- `rechentafel.poi/load-workbook` — reads cells, formulas, tables,
  defined names
- `rechentafel.poi-writer/save-workbook` — full dynamic-array round-trip
  including `cm="1"` cell metadata + `xl/metadata.xml` part

### Reproducibility
- Seedable PRNG: `(assoc wb :rng-seed N)` makes RAND / RANDARRAY /
  RANDBETWEEN draws deterministic for the duration of recalc
- Workbook is fully persistent: forking via `set-cell` shares structure
  with the parent

### Benchmarks
- Six shapes (chain, wide, aggregate, star, spill, lambda-rec) running
  on JVM and cljs
- Comparator vs Apache POI (`FormulaEvaluator`) and LibreOffice
  (`--convert-to xlsx`)
- Cross-engine result verification: spread-clj, POI, LibreOffice, and
  the analytical expected values agree on every comparable shape
