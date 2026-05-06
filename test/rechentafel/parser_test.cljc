(ns rechentafel.parser-test
  "Parser tests ported from Apache POI's `TestFormulaParser` where the
  assertion is about parse *shape* rather than Ptg byte layout. Tests that
  depend on the HSSF workbook model (named ranges, 3D refs, external
  workbooks, `.`/`..` range syntax, union/intersection) are deferred.

  Shape convention: the parser attaches `:pos` and `:text` metadata to
  tokens; tests compare against normalised ASTs with that metadata stripped
  via `strip` below.

  POI source:
    ../poi/poi/src/test/java/org/apache/poi/hssf/model/TestFormulaParser.java

  Divergence note: POI fuses one layer of unary-minus into a NumberPtg
  literal (e.g. `-3` → `NumberPtg(-3)`). We keep unary ops as explicit AST
  nodes; semantics are identical, only the encoding differs."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [clojure.walk :as walk]
            [rechentafel.parser :as p]))

(defn- strip
  "Remove lexer-provenance keys so we compare AST shape only."
  [ast]
  (walk/postwalk
   (fn [n] (if (map? n) (dissoc n :pos :text :ws-before?) n))
   ast))

(defn- ast [s] (strip (p/parse s)))

(defn- throws? [s]
  (try (p/parse s) false
       (catch #?(:clj Exception :cljs :default) _ true)))

;; ---------------------------------------------------------------------------
;; Shape constructors

(defn- N    [v]       {:op :num   :value (double v)})
(defn- S    [v]       {:op :str   :value v})
(defn- B    [v]       {:op :bool  :value v})
(defn- E    [v]       {:op :err   :value v})
(defn- Ref  [col row & {:keys [abs-col? abs-row? sheet]
                        :or   {abs-col? false abs-row? false}}]
  (cond-> {:op :ref :col col :row row
           :abs-col? abs-col? :abs-row? abs-row?}
    sheet (assoc :sheet sheet)))
(defn- Bin  [sym L R] {:op :binop  :sym sym :left L :right R})
(defn- Un   [sym A]   {:op :unop   :sym sym :arg A})
(defn- Pct  [A]       {:op :postop :sym :percent :arg A})
(defn- Rng  [L R]     {:op :range  :left L :right R})
(defn- Call [n & args] {:op :call  :name n :args (vec args)})
(defn- Nm   [n]       {:op :name   :value n})
(defn- Arr  [& rows]  {:op :array  :rows (vec rows)})

;; ---------------------------------------------------------------------------
;; POI: testSimpleFormula, testFormulaWithSpace1, testLargeInt, etc.

(deftest scalars-and-basic-arithmetic
  (is (= (Bin :plus (N 2) (N 2))        (ast "2+2")))
  (is (= (Bin :plus (N 2) (N 2))        (ast " 2 + 2 ")))      ; whitespace
  (is (= (N 40)                          (ast "40")))
  (is (= (N 40000)                       (ast "40000")))
  (is (= (N 65535)                       (ast "65535")))
  (is (= (N 65536)                       (ast "65536")))
  (is (= (N 65534.6)                     (ast "65534.6")))
  (is (= (Bin :div (N 40000) (N 2))      (ast "40000/2")))
  (is (= (Bin :pow (N 2) (N 5))          (ast "2^5"))))

(deftest booleans-and-strings
  (is (= (B true)  (ast "TRUE")))
  (is (= (B false) (ast "FALSE")))
  (is (= (B true)  (ast "true")))                ; POI: case-insensitive
  (is (= (S "hello world") (ast "\"hello world\"")))
  (is (= (S "  hi  ")      (ast "\"  hi  \"")))  ; leading/trailing ws preserved
  (is (= (S "")            (ast "\"\"")))
  (is (= (S "\"")          (ast "\"\"\"\"")))    ; single embedded quote
  (is (= (S "test\"ing")   (ast "\"test\"\"ing\""))))

;; POI: testFormulaWithString, testConcatenate, testNonAlphaFormula
(deftest concat-and-calls
  (is (= (Bin :concat (S "hello") (S "world"))
         (ast "\"hello\" & \"world\" ")))
  (is (= (Call "CONCATENATE" (S "first") (S "second"))
         (ast "CONCATENATE(\"first\",\"second\")")))
  (is (= (Bin :concat
              (Bin :concat (S "TOTAL[") (Ref 5 2))
              (S "]"))
         (ast "\"TOTAL[\"&F3&\"]\""))))

;; POI: testEmbeddedSlash
(deftest embedded-slash-in-string
  (is (= (Call "HYPERLINK" (S "http://www.jakarta.org") (S "Jakarta"))
         (ast "HYPERLINK(\"http://www.jakarta.org\",\"Jakarta\")"))))

;; POI: testSumIf, testParseSumIfSum, testFormulaWithSpaceNRef,
;; testLookupAndMatchFunctionArgs
(deftest function-args-and-ranges
  (is (= (Call "SUMIF" (Rng (Ref 0 0) (Ref 0 4)) (S ">4000")
               (Rng (Ref 1 0) (Ref 1 4)))
         (ast "SUMIF(A1:A5,\">4000\",B1:B5)")))
  (is (= (Call "sum" (Rng (Ref 0 1) (Ref 0 2)))
         (ast "sum( A2:A3 )")))
  (is (= (Call "lookup" (Ref 0 0)
               (Rng (Ref 0 2) (Ref 0 51))
               (Rng (Ref 1 2) (Ref 1 51)))
         (ast "lookup(A1, A3:A52, B3:B52)")))
  (is (= (Bin :plus (N 2) (Call "sum" (N 3) (N 4)))
         (ast "2+ sum( 3 , 4) "))))

;; POI: testUnaryMinus, testUnaryPlus, testExactEncodingOfUnaryPlusAndMinus
;; Semantic equivalent shape — we don't fuse unary into literals.
(deftest unary-operators
  (is (= (Un :minus (Ref 0 0))          (ast "-A1")))
  (is (= (Un :plus  (Ref 0 0))          (ast "+A1")))
  (is (= (Un :minus (N 3))              (ast "-3")))
  (is (= (Un :minus (Un :minus (N 4)))  (ast "--4")))
  (is (= (Un :plus  (Un :plus  (Un :plus (N 5))))
         (ast "+++5")))
  (is (= (Un :plus  (Un :plus  (Un :minus (N 6))))
         (ast "++-6")))
  (is (= (Un :plus  (N 12))             (ast "+ 12")))
  (is (= (Un :minus (N 13))             (ast "- 13"))))

;; POI: testExponentialParsing, testNumbers
(deftest number-literal-shapes
  (is (= (Bin :div (N 1.3e21)  (N 2)) (ast "1.3E21/2")))
  (is (= (Bin :div (N 1322e21) (N 2)) (ast "1322E21/2")))
  (is (= (Bin :div (N 13.0)    (N 2)) (ast "1.3E1/2")))
  (is (= (N 0.1)                      (ast ".1")))
  (is (= (Un :plus  (N 0.1))          (ast "+.1")))
  (is (= (Un :minus (N 0.1))          (ast "-.1")))
  (is (= (N 100.0)                    (ast "10E1")))
  (is (= (N 100.0)                    (ast "10E+1")))
  (is (= (N 1.0)                      (ast "10E-1"))))

;; POI: testPercent
(deftest percent-postfix
  (is (= (Pct (N 5))                          (ast "5%")))
  (is (= (Pct (N 250))                        (ast " 250 % ")))
  (is (= (Pct (Pct (N 12345.678)))            (ast "12345.678%%")))
  (is (= (Bin :mul
              (Pct (Bin :plus (Ref 0 0) (N 35)))
              (Pct (Ref 1 0)))
         (ast "(A1+35)%*B1%")))
  (is (= (Pct (S "8.75"))                     (ast "\"8.75\"%")))
  ;; POI: percent tighter than ^  (`50%^3` → (50%)^3)
  (is (= (Bin :pow (Pct (N 50)) (N 3))        (ast "50%^3")))
  (is (= (Pct (S "abc"))                      (ast "\"abc\"%")))
  (is (= (Pct (E :na))                        (ast "#N/A%"))))

;; POI: testPrecedenceAndAssociativity, testPower
(deftest precedence-and-associativity
  ;; comparison is left-associative
  (is (= (Bin :eq (Bin :eq (Bin :eq (B true) (B true)) (N 2)) (N 2))
         (ast "TRUE=TRUE=2=2")))
  ;; POI: `^` left-associative  (2^3^2 = (2^3)^2 = 64)
  (is (= (Bin :pow (Bin :pow (N 2) (N 3)) (N 2))
         (ast "2^3^2")))
  ;; `&` lower than `+`  →  "abc" & (2+3) & "def", & left-assoc
  (is (= (Bin :concat
              (Bin :concat (S "abc") (Bin :plus (N 2) (N 3)))
              (S "def"))
         (ast "\"abc\"&2+3&\"def\"")))
  ;; multiplicatives left-assoc, mul/div > add/sub
  (is (= (Bin :minus
              (Bin :div (N 1) (N 2))
              (Bin :mul (N 3) (N 4)))
         (ast "1/2-3*4")))
  ;; `^` tighter than `*`:  2*(2^2)
  (is (= (Bin :mul (N 2) (Bin :pow (N 2) (N 2)))
         (ast "2*2^2")))
  ;; `%` tighter than `^`:  2^(200%)
  (is (= (Bin :pow (N 2) (Pct (N 200)))
         (ast "2^200%"))))

;; POI: testParseErrorLiterals
(deftest error-literals
  (is (= (E :null)  (ast "#NULL!")))
  (is (= (E :div0)  (ast "#DIV/0!")))
  (is (= (E :value) (ast "#VALUE!")))
  (is (= (E :ref)   (ast "#REF!")))
  (is (= (E :name)  (ast "#NAME?")))
  (is (= (E :num)   (ast "#NUM!")))
  (is (= (E :na)    (ast "#N/A")))
  ;; POI: HLOOKUP(F7,#REF!,G7,#REF!)  — errors valid as function args
  (is (= (Call "HLOOKUP" (Ref 5 6) (E :ref) (Ref 6 6) (E :ref))
         (ast "HLOOKUP(F7,#REF!,G7,#REF!)"))))

;; POI: testComparisonInParen
(deftest parenthesised-comparison
  (is (= (Bin :gt (Ref 0 0) (Ref 1 1))
         (ast "(A1 > B2)"))))

;; POI: testParseArray, testParseStringElementInArray, testParseArrayNegativeElement
(deftest array-literals
  (is (= (Call "mode"
               (Arr [(N 1) (N 2) (N 2) (E :ref)]
                    [(B false) (N 3) (N 3) (N 2)]))
         (ast "mode({1,2,2,#REF!;FALSE,3,3,2})")))
  (is (= (Call "MAX" (Arr [(S "5")]) (N 3))
         (ast "MAX({\"5\"},3)")))
  (is (= (Arr [(Un :minus (N 42))])
         (ast "{-42}")))
  (is (= (Arr [(Un :minus (N 5))])
         (ast "{- 5}"))))

;; POI: testRangeOperator (the subset that doesn't round-trip through POI's
;; formula-string normaliser — we only check *parse*, not canonical render)
(deftest range-operator
  (is (= (Rng (Ref 1 3 :abs-row? true) (Ref 2 0 :abs-col? true))
         (ast "B$4:$C1")))
  ;; sheet-qualified on each side
  (is (= (Rng (Ref 1 3 :abs-row? true :sheet "Sheet1")
              (Ref 2 0 :abs-col? true :sheet "Sheet1"))
         (ast "Sheet1!B$4:Sheet1!$C1"))))

;; POI: testFuncPtgSelection, testParseErrorTypeFunction
(deftest function-name-forms
  (is (= (Call "countif" (Rng (Ref 0 0) (Ref 0 1)) (N 1))
         (ast "countif(A1:A2, 1)")))
  (is (= (Call "sin" (N 1))
         (ast "sin(1)")))
  ;; function name containing '.'
  (is (= (Call "error.type" (Ref 0 0))
         (ast "error.type(A1)"))))

;; POI: testParserErrors — these should throw.
;; We skip ones that depend on POI's error-message contents.
(deftest parse-errors
  (is (throws? " 12 . 345  "))     ; two dots in a number
  (is (throws? "(")                ; unmatched open paren
      "unmatched opening paren throws")
  (is (throws? ")"))                ; bare close paren
  (is (throws? "42+"))              ; trailing binop
  (is (throws? "IF(")))             ; unterminated call

;; POI: testMultiSheetReference / testBooleanNamedSheet — quoted sheet names
(deftest quoted-sheet-names
  (is (= (Ref 0 0 :sheet "Test Sheet")
         (ast "'Test Sheet'!A1")))
  (is (= (Ref 1 1 :sheet "true")
         (ast "'true'!B2"))))

;; ---------------------------------------------------------------------------
;; Coverage beyond POI tests

;; Range binds tighter than unary — this is the Excel semantics but easy to
;; get wrong with a naïve Pratt.
(deftest range-binds-tighter-than-unary
  (is (= (Un :minus (Rng (Ref 0 0) (Ref 1 4)))
         (ast "-A1:B5")))
  (is (= (Un :minus (Pct (Rng (Ref 0 0) (Ref 1 4))))
         (ast "-A1:B5%")))
  ;; range chains left-assoc
  (is (= (Rng (Rng (Ref 0 0) (Ref 1 4)) (Ref 2 9))
         (ast "A1:B5:C10"))))

;; Whole-column and whole-row refs
(defn- Wcol [col & {:keys [abs-col?] :or {abs-col? false}}]
  {:op :ref :whole :col :col col :abs-col? abs-col?})
(defn- Wrow [row & {:keys [abs-row?] :or {abs-row? false}}]
  {:op :ref :whole :row :row row :abs-row? abs-row?})

(defn- Union [& args] {:op :union :args (vec args)})

;; POI: testUnionInParen
(deftest union-operator
  (is (= (Union (Rng (Ref 0 0) (Ref 1 1)) (Rng (Ref 1 1) (Ref 2 2)))
         (ast "(A1:B2,B2:C3)")))
  ;; Inside a function call, comma still separates args (not union)
  (is (= (Call "SUM" (Ref 0 0) (Ref 1 0))
         (ast "SUM(A1, B1)")))
  ;; Parenthesised scalar comparison stays plain (no union)
  (is (= (Bin :gt (Ref 0 0) (Ref 1 1))
         (ast "(A1 > B2)")))
  ;; Three-way union
  (is (= (Union (Ref 0 0) (Ref 1 0) (Ref 2 0))
         (ast "(A1, B1, C1)"))))

;; POI: testMissingArgs
(deftest missing-args
  (let [M {:op :missing}]
    (is (= (Call "IF" (Ref 0 0) M (Ref 2 0))
           (ast "IF(A1, ,C1)")))
    (is (= (Call "counta" M (Rng (Ref 0 0) (Ref 1 1)) M)
           (ast "counta( , A1:B2, )")))
    (is (= (Call "foo" M M)
           (ast "foo(,)")))
    (is (= (Call "bar")
           (ast "bar()")))))

(deftest whole-column-and-row-refs
  (is (= (Rng (Wcol 0) (Wcol 0))                    (ast "A:A")))
  (is (= (Rng (Wcol 0) (Wcol 2))                    (ast "A:C")))
  (is (= (Rng (Wcol 0 :abs-col? true) (Wcol 1))     (ast "$A:B")))
  (is (= (Rng (Wrow 0) (Wrow 0))                    (ast "1:1")))
  (is (= (Rng (Wrow 0) (Wrow 4))                    (ast "1:5")))
  (is (= (Rng (Wrow 0 :abs-row? true) (Wrow 1 :abs-row? true))
         (ast "$1:$2"))))

;; POI: testFormulaWithSpace2 — whitespace around commas and inside parens
;; POI asserts a 5-Ptg count; we assert the parse shape instead.
(deftest whitespace-inside-function-call
  (is (= (Bin :plus (N 2) (Call "sum" (N 3) (N 4)))
         (ast "2+ sum( 3 , 4) "))))

;; POI: testNamesWithUnderscore — identifiers with leading/embedded
;; underscores must stick together. A naïve lexer would split
;; `DA6_LEO_...` into ref `DA6` + name `_LEO_...`.
(deftest underscore-in-names
  (is (= (Bin :mul (Nm "DA6_LEO_WBS_Number") (N 2))
         (ast "DA6_LEO_WBS_Number*2")))
  (is (= (Bin :div
              (Bin :plus (Bin :mul (Nm "A1_") (Nm "_A1")) (Nm "A_1"))
              (Nm "A_1_"))
         (ast "(A1_*_A1+A_1)/A_1_")))
  (is (= (Call "INDEX" (Nm "DA6_LEO_WBS_Name")
               (Call "MATCH" (Ref 0 2 :abs-col? true)
                     (Nm "DA6_LEO_WBS_Number") (N 0)))
         (ast "INDEX(DA6_LEO_WBS_Name,MATCH($A3,DA6_LEO_WBS_Number,0))"))))

;; POI: testZeroRowRefs — `B0` is not a valid cell (rows are 1-indexed).
;; POI falls back to a named-range lookup; with no workbook we parse it as
;; a bare identifier reference. Leading zeros on the row component are
;; stripped (`B000001` → B1).
(deftest zero-row-refs
  (is (= (Nm "B0") (ast "B0")))
  (is (= (Ref 1 0) (ast "B000001"))))

;; POI: testIntersection / testIntersectionInParen / testIntersectionInFunctionArgs.
;; Space between reference expressions is the intersection operator.
(defn- Isect [L R] {:op :intersect :left L :right R})

(deftest intersection-operator
  ;; Bare top-level intersection
  (is (= (Isect (Rng (Ref 0 0) (Ref 1 1)) (Rng (Ref 1 1) (Ref 2 2)))
         (ast "A1:B2 B2:C3")))
  ;; Inside parens
  (is (= (Isect (Rng (Ref 0 0) (Ref 1 1)) (Rng (Ref 1 1) (Ref 2 2)))
         (ast "(A1:B2 B2:C3)")))
  ;; As the single argument of a function call
  (is (= (Call "SUM" (Isect (Rng (Ref 0 0) (Ref 1 1)) (Rng (Ref 1 1) (Ref 2 2))))
         (ast "SUM(A1:B2 B2:C3)")))
  ;; POI note: `1 2` parses as intersection too (left-assoc, space operand)
  (is (= (Isect (N 1) (N 2))                       (ast "1 2")))
  ;; Chained intersection is left-associative (like range, `a b c` = (a b) c)
  (is (= (Isect (Isect (Rng (Ref 0 0) (Ref 1 1)) (Rng (Ref 1 1) (Ref 2 2)))
                (Rng (Ref 2 2) (Ref 3 3)))
         (ast "A1:B2 B2:C3 C3:D4")))
  ;; Mixes with function calls (POI testIntersection — the 3D sheet prefix
  ;; is preserved on each leg).
  (let [r11 (Ref 1 1 :abs-col? true :abs-row? true :sheet "Sheet1")
        r22 (Ref 2 2 :abs-col? true :abs-row? true :sheet "Sheet1")
        r41 (Ref 4 1 :abs-col? true :abs-row? true :sheet "Sheet1")
        r43 (Ref 4 3 :abs-col? true :abs-row? true :sheet "Sheet1")
        r00 (Ref 0 0 :abs-col? true :abs-row? true :sheet "Sheet1")
        r35 (Ref 3 5 :abs-col? true :abs-row? true :sheet "Sheet1")]
    (is (= (Isect (Isect (Rng r11 r22)
                         (Call "OFFSET" (Rng r41 r43) (N 1) r00))
                  r35)
           (ast "Sheet1!$B$2:$C$3 OFFSET(Sheet1!$E$2:$E$4, 1,Sheet1!$A$1) Sheet1!$D$6")))))

;; POI: testRangeFuncOperand_bug46951 — range where one side is a function
;; call. Our `parse-range` uses `parse-atom` on each side, and `parse-atom`
;; treats `ident(` as a call, so this works out of the box.
(deftest range-with-function-operand
  (is (= (Call "SUM"
               (Rng (Ref 2 0)
                    (Call "OFFSET" (Ref 2 0) (N 0) (Ref 1 0))))
         (ast "SUM(C1:OFFSET(C1,0,B1))"))))

;; POI: testRange_bug46643 / testExplicitRangeWithTwoSheetNames — both
;; sides of the range can carry an explicit sheet prefix.
(deftest range-with-explicit-sheet-on-both-sides
  (is (= (Rng (Ref 0 0 :sheet "Sheet1") (Ref 1 2 :sheet "Sheet1"))
         (ast "Sheet1!A1:Sheet1!B3")))
  (is (= (Rng (Ref 5 0 :sheet "Sheet1") (Ref 6 1 :sheet "Sheet1"))
         (ast "Sheet1!F1:Sheet1!G2"))))

;; POI: testUnionOfFullCollFullRowRef — whole-col/row ranges at top level
;; and in unions. `3:4` is two-row range, `$Z:$AC` a four-column range.
(deftest whole-col-row-unions-and-ranges
  (is (= (Rng (Wrow 2) (Wrow 3))                            (ast "3:4")))
  (is (= (Rng (Wcol 25 :abs-col? true) (Wcol 28 :abs-col? true))
         (ast "$Z:$AC")))
  (is (= (Rng (Wcol 1) (Wcol 1))                            (ast "B:B")))
  (is (= (Rng (Wrow 10 :abs-row? true) (Wrow 12 :abs-row? true))
         (ast "$11:$13")))
  ;; Union of two whole-col / whole-row ranges at top level
  (is (= (Union (Rng (Wcol 0 :abs-col? true) (Wcol 0 :abs-col? true))
                (Rng (Wrow 0 :abs-row? true) (Wrow 3 :abs-row? true)))
         (ast "$A:$A,$1:$4"))))

;; POI: testParseSheetNameWithMultipleSingleQuotes — `''''` inside the
;; quoted sheet name decodes to `''` (two apostrophes).
(deftest quoted-sheet-with-escaped-quotes
  (let [lc (assoc (Wcol 0 :abs-col? true) :sheet "Sh''t1")
        rr (assoc (Wrow 0 :abs-row? true) :sheet "Sh''t1")
        rr2 (assoc (Wrow 3 :abs-row? true) :sheet "Sh''t1")]
    (is (= (Rng lc lc) (ast "'Sh''''t1'!$A:$A")))
    (is (= (Union (Rng lc lc) (Rng rr rr2))
           (ast "'Sh''''t1'!$A:$A,'Sh''''t1'!$1:$4")))))

;; POI: testParseNumber — integer-vs-double boundary (POI uses Int vs Number
;; Ptg at 65535/65536; we normalise all numbers to double, so just confirm
;; the values parse round-trip).
(deftest number-boundaries
  (is (= (N 65535)                      (ast "65535")))
  (is (= (N 65536)                      (ast "65536")))
  ;; We keep `-1` as (Un :minus 1), not the POI-fused IntPtg(-1).
  (is (= (Un :minus (N 1))              (ast "-1"))))

;; POI: testRanges — dot-notation ranges (`A1.A2`, `A1..A2`, `A1...A2`).
;; These are a POI/Excel-legacy convention we do not support.
(deftest dot-ranges-unsupported
  (is (throws? "A1.A2"))
  (is (throws? "A1..A2"))
  (is (throws? "A1...A2")))

;; POI: testEdgeCaseParserErrors subset — we skip cases whose messages rely
;; on a workbook context, but these are pure-grammar rejects.
(deftest edge-case-parser-errors
  ;; RHS of range must be a reference
  (is (throws? "A1:1"))
  ;; Garbage after sheet bang
  (is (throws? "Sheet1!!!"))
  (is (throws? "Sheet1!.Name")))

;; POI: testMultiSheetReference — 3D sheet ranges `Sheet1:Sheet3!A1`. The
;; second sheet name is carried on every ref inside the range as
;; `:last-sheet`, mirroring POI's Ref3DPxg / Area3DPxg structure.
(defn- Ref3D [col row sheet last-sheet & {:keys [abs-col? abs-row?]
                                          :or   {abs-col? false abs-row? false}}]
  {:op :ref :col col :row row :abs-col? abs-col? :abs-row? abs-row?
   :sheet sheet :last-sheet last-sheet})

(deftest three-d-sheet-ranges
  (is (= (Ref3D 0 0 "Sheet1" "Sheet3")
         (ast "Sheet1:Sheet3!A1")))
  (is (= (Rng (Ref3D 0 0 "Sheet1" "Sheet3") (Ref3D 1 4 "Sheet1" "Sheet3"))
         (ast "Sheet1:Sheet3!A1:B5")))
  ;; Quoted sheet names on both sides
  (is (= (Ref3D 0 0 "My Sheet" "Other")
         (ast "'My Sheet':'Other'!A1")))
  ;; 3D whole-column range — both corners pick up :last-sheet too
  (is (= (Rng (assoc (Wcol 0) :sheet "Sheet1" :last-sheet "Sheet3")
              (assoc (Wcol 0) :sheet "Sheet1" :last-sheet "Sheet3"))
         (ast "Sheet1:Sheet3!A:A"))))

;; POI: testParseExternalWorkbookReference — `[Book1]Sheet1!A1` and the
;; indexed form `[1]Sheet1!A1`. Workbook is a string field on the ref.
(defn- RefExt [col row workbook sheet & {:keys [last-sheet]}]
  (cond-> {:op :ref :col col :row row :abs-col? false :abs-row? false
           :workbook workbook :sheet sheet}
    last-sheet (assoc :last-sheet last-sheet)))

(deftest external-workbook-refs
  (is (= (RefExt 0 0 "Book1.xlsx" "Sheet1")
         (ast "[Book1.xlsx]Sheet1!A1")))
  (is (= (RefExt 0 0 "Book1" "Sheet1")
         (ast "[Book1]Sheet1!A1")))
  (is (= (RefExt 0 0 "1" "Sheet1")
         (ast "[1]Sheet1!A1")))
  ;; External + 3D combined
  (is (= (RefExt 0 0 "Book1" "Sheet1" :last-sheet "Sheet3")
         (ast "[Book1]Sheet1:Sheet3!A1")))
  ;; External range — workbook propagates across both endpoints
  (is (= (Rng (RefExt 0 0 "Book1" "Sheet1")
              (RefExt 1 4 "Book1" "Sheet1"))
         (ast "[Book1]Sheet1!A1:B5"))))

;; POI: StructuredReference grammar — `Table1[Col]`, `Table1[@Col]`,
;; `Table1[#Headers]`, `Table1[[Col1]:[Col2]]`, `Table1[[#Hdr],[Col]]`.
(defn- Tref [table & specs]
  {:op :table-ref :table table :specifiers (vec specs)})

(deftest structured-table-refs
  (is (= (Tref "Table1" {:kind :column :name "Column1"})
         (ast "Table1[Column1]")))
  (is (= (Tref "Table1")
         (ast "Table1[]")))
  ;; `@` expands to an implicit `#This Row` specifier
  (is (= (Tref "Table1" {:kind :area :value :this-row}
               {:kind :column :name "Column1"})
         (ast "Table1[@Column1]")))
  (is (= (Tref "Table1" {:kind :area :value :headers})
         (ast "Table1[#Headers]")))
  (is (= (Tref "Table1" {:kind :area :value :all})
         (ast "Table1[#All]")))
  (is (= (Tref "Table1" {:kind :area :value :data})
         (ast "Table1[#Data]")))
  (is (= (Tref "Table1" {:kind :area :value :totals})
         (ast "Table1[#Totals]")))
  (is (= (Tref "Table1" {:kind :column-range :from "Col1" :to "Col2"})
         (ast "Table1[[Col1]:[Col2]]")))
  (is (= (Tref "Table1"
               {:kind :area :value :headers}
               {:kind :column :name "Col1"})
         (ast "Table1[[#Headers],[Col1]]")))
  (is (= (Tref "Table1"
               {:kind :area :value :this-row}
               {:kind :column-range :from "Col1" :to "Col2"})
         (ast "Table1[@[Col1]:[Col2]]")))
  ;; Implicit-table form (used inside a formula that lives in the table)
  (is (= (Tref nil {:kind :column :name "Column1"})
         (ast "[Column1]")))
  ;; Table ref as a function argument
  (is (= (Call "SUM" (Tref "Table1" {:kind :column :name "Col1"}))
         (ast "SUM(Table1[Col1])")))
  ;; Table ref on both sides of a range
  (is (= (Rng (Tref "Table1" {:kind :column :name "A"})
              (Tref "Table2" {:kind :column :name "B"}))
         (ast "Table1[A]:Table2[B]")))
  ;; Unknown # specifier throws
  (is (throws? "Table1[#Nope]")))

;; ---------------------------------------------------------------------------
;; Structured error data + format-error diagnostic

(defn- parse-error [s]
  (try (p/parse s) nil
       (catch #?(:clj Exception :cljs :default) e (ex-data e))))

(deftest structured-error-data
  (testing "carries :type :parse-error, :line, :col, :offset, :source"
    (let [e (parse-error "SUM(A1+")]
      (is (= :parse-error (:type e)))
      (is (= 1 (:line e)))
      (is (= 8 (:col e)))                ; just past the trailing `+`
      (is (= "SUM(A1+" (:source e)))
      (is (string? (:expected e)))
      (is (string? (:got e)))))
  (testing "caret lands on offending token, not past it"
    ;; A whitespace-separated trailing identifier would now bind as
    ;; intersection (`2+2 extra` = `2+2` intersect `extra`). Use `}`
    ;; instead — not ref-starting, so the trailing-token check fires.
    (let [e (parse-error "2*2 }")]
      (is (= 5 (:col e)))                ; '}' is at col 5
      (is (= "end of formula" (:expected e)))))
  (testing "multi-line input is line/col accurate"
    (let [e (parse-error "A1+\nB1+\nC1+")]
      (is (= 3 (:line e)))
      (is (= 4 (:col e))))))

(deftest format-error-render
  (let [rendered (p/format-error (parse-error "SUM(A1+"))]
    (is (re-find #"Parse error at line 1, col 8" rendered))
    (is (re-find #"SUM\(A1\+" rendered))
    (is (re-find #"(?m)^ {9}\^" rendered)    ; 2-space indent + 7 leading spaces + caret
        "caret lands under col 8 (last char of SUM(A1+)"))
  (let [rendered (p/format-error (parse-error "1+)"))]
    (is (re-find #"but got '\)'" rendered))))
