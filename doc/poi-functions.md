# Apache POI Formula Function Inventory
Complete inventory of 413 Excel formula functions registered in Apache POI.
Last updated: 2026-04-17 (POI ~5.x)

## Math (61)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| ABS | 1 | strict | NumericFunction.ABS |  |
| ACOS | 1 | strict | NumericFunction.ACOS |  |
| ACOSH | 1 | strict | NumericFunction.ACOSH |  |
| ASIN | 1 | strict | NumericFunction.ASIN |  |
| ASINH | 1 | strict | NumericFunction.ASINH |  |
| ATAN | 1 | strict | NumericFunction.ATAN |  |
| ATAN2 | 2 | strict | NumericFunction.ATAN2 |  |
| ATANH | 1 | strict | NumericFunction.ATANH |  |
| AVEDEV | 1-30 | strict | AggregateFunction.AVEDEV |  |
| CEILING | 2 | strict | NumericFunction.CEILING |  |
| CEILING.MATH | varies | strict | CeilingMath.instance | ATP |
| CEILING.PRECISE | varies | strict | CeilingPrecise.instance | ATP |
| COMBIN | 2 | strict | NumericFunction.COMBIN |  |
| COS | 1 | strict | NumericFunction.COS |  |
| COSH | 1 | strict | NumericFunction.COSH |  |
| DEGREES | 1 | strict | NumericFunction.DEGREES |  |
| EVEN | 1 | strict | NumericFunction.EVEN |  |
| EXP | 1 | strict | NumericFunction.EXP |  |
| FACT | 1 | strict | NumericFunction.FACT |  |
| FACTDOUBLE | varies | strict | FactDouble.instance | ATP |
| FLOOR | 2 | strict | NumericFunction.FLOOR |  |
| FLOOR.MATH | varies | strict | FloorMath.instance | ATP |
| FLOOR.PRECISE | varies | strict | FloorPrecise.instance | ATP |
| FREQUENCY | 2 | strict | Frequency.instance | array-aware |
| GCD | varies | strict | Gcd.instance | ATP |
| INT | 1 | strict | NumericFunction.INT |  |
| LCM | varies | strict | Lcm.instance | ATP |
| LN | 1 | strict | NumericFunction.LN |  |
| LOG | 1-2 | strict | NumericFunction.LOG |  |
| LOG10 | 1 | strict | NumericFunction.LOG10 |  |
| MDETERM | 1 | strict | MatrixFunction.MDETERM |  |
| MINVERSE | 1 | strict | MatrixFunction.MINVERSE |  |
| MMULT | 2 | strict | MatrixFunction.MMULT | array-aware |
| MOD | 2 | strict | NumericFunction.MOD |  |
| MROUND | varies | strict | MRound.instance | ATP |
| ODD | 1 | strict | NumericFunction.ODD |  |
| PERMUT | 2 | strict | NotImplementedFunction |  |
| PI | 0 | strict | NumericFunction.PI |  |
| POWER | 2 | strict | NumericFunction.POWER |  |
| PRODUCT | 0-30 | strict | AggregateFunction.PRODUCT |  |
| QUOTIENT | varies | strict | Quotient.instance | ATP |
| RADIANS | 1 | strict | NumericFunction.RADIANS |  |
| RANDBETWEEN | varies | strict | RandBetween.instance | ATP |
| ROUND | 2 | strict | NumericFunction.ROUND |  |
| ROUNDDOWN | 2 | strict | NumericFunction.ROUNDDOWN |  |
| ROUNDUP | 2 | strict | NumericFunction.ROUNDUP |  |
| SIGN | 1 | strict | NumericFunction.SIGN |  |
| SIN | 1 | strict | NumericFunction.SIN |  |
| SINH | 1 | strict | NumericFunction.SINH |  |
| SQRT | 1 | strict | NumericFunction.SQRT |  |
| SQRTPI | varies | strict | Sqrtpi.instance | ATP |
| SUM | 0-30 | strict | NotImplementedFunction |  |
| SUMPRODUCT | 1-30 | strict | Sumproduct() | array-aware |
| SUMSQ | 0-30 | strict | AggregateFunction.SUMSQ |  |
| SUMX2MY2 | 2 | strict | Sumx2my2() | array-aware |
| SUMX2PY2 | 2 | strict | Sumx2py2() | array-aware |
| SUMXMY2 | 2 | strict | Sumxmy2() | array-aware |
| TAN | 1 | strict | NumericFunction.TAN |  |
| TANH | 1 | strict | NumericFunction.TANH |  |
| TRANSPOSE | 1 | strict | MatrixFunction.TRANSPOSE | array-aware |
| TRUNC | 1-2 | strict | NumericFunction.TRUNC |  |

## Text (38)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| ASC | 1 | strict | NotImplementedFunction |  |
| CHAR | 1 | strict | TextFunction.CHAR |  |
| CLEAN | 1 | strict | TextFunction.CLEAN |  |
| CODE | 1 | strict | Code() |  |
| CONCAT | varies | strict | TextFunction.CONCAT | ATP |
| CONCATENATE | 0-30 | strict | TextFunction.CONCATENATE |  |
| DBCS | 1 | strict | NotImplementedFunction |  |
| DOLLAR | 1-2 | strict | NumericFunction.DOLLAR |  |
| EXACT | 2 | strict | TextFunction.EXACT |  |
| FIND | 2-3 | strict | TextFunction.FIND |  |
| FINDB | 2-3 | strict | NotImplementedFunction |  |
| FIXED | 1-3 | strict | Fixed() |  |
| JIS | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| LEFT | 1-2 | strict | TextFunction.LEFT |  |
| LEFTB | 1-2 | strict | NotImplementedFunction |  |
| LEN | 1 | strict | TextFunction.LEN |  |
| LENB | 1 | strict | NotImplementedFunction |  |
| LOWER | 1 | strict | TextFunction.LOWER |  |
| MID | 3 | strict | TextFunction.MID |  |
| MIDB | 3 | strict | NotImplementedFunction |  |
| N | 1 | strict | NotImplementedFunction |  |
| NUMBERVALUE | varies | strict | NumberValueFunction.instance | ATP |
| PROPER | 1 | strict | TextFunction.PROPER |  |
| REPLACE | 4 | strict | Replace() |  |
| REPLACEB | 4 | strict | NotImplementedFunction |  |
| REPT | 2 | strict | Rept() |  |
| RIGHT | 1-2 | strict | TextFunction.RIGHT |  |
| RIGHTB | 1-2 | strict | NotImplementedFunction |  |
| ROMAN | 1-2 | strict | Roman() |  |
| SEARCH | 2-3 | strict | TextFunction.SEARCH |  |
| SEARCHB | 2-3 | strict | NotImplementedFunction |  |
| SUBSTITUTE | 3-4 | strict | Substitute() |  |
| T | 1 | strict | T() |  |
| TEXT | 2 | strict | TextFunction.TEXT |  |
| TEXTJOIN | varies | strict | TextJoinFunction.instance | ATP |
| TRIM | 1 | strict | TextFunction.TRIM |  |
| UPPER | 1 | strict | TextFunction.UPPER |  |
| VALUE | 1 | strict | Value() |  |

## Logical (11)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| AND | 1-30 | lazy | BooleanFunction.AND |  |
| CHOOSE | 2-30 | lazy | NotImplementedFunction |  |
| FALSE | 0 | strict | BooleanFunction.FALSE |  |
| IF | 2-3 | lazy | NotImplementedFunction |  |
| IFERROR | varies | lazy | IfError.instance | ATP |
| IFNA | varies | lazy | IfNa.instance | ATP |
| IFS | varies | lazy | Ifs.instance | ATP |
| NOT | 1 | strict | BooleanFunction.NOT |  |
| OR | 1-30 | lazy | BooleanFunction.OR |  |
| SWITCH | varies | lazy | Switch.instance | ATP |
| TRUE | 0 | strict | BooleanFunction.TRUE |  |

## Lookup (11)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| GETPIVOTDATA | 2-30 | strict | NotImplementedFunction |  |
| HLOOKUP | 3-4 | strict | Hlookup() |  |
| HYPERLINK | 1-2 | strict | Hyperlink() |  |
| INDEX | 2-4 | strict | Index() |  |
| INDIRECT | 1-2 | strict | NotImplementedFunction | volatile |
| LOOKUP | 2-3 | strict | Lookup() |  |
| MATCH | 2-3 | strict | Match() |  |
| OFFSET | 3-5 | strict | NotImplementedFunction | volatile |
| VLOOKUP | 3-4 | strict | Vlookup() |  |
| XLOOKUP | varies | strict | XLookupFunction.instance | ATP |
| XMATCH | varies | strict | XMatchFunction.instance | ATP |

## Date/Time (23)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| DATE | 3 | strict | DateFunc.instance |  |
| DATEDIF | 3 | strict | NotImplementedFunction |  |
| DATEVALUE | 1 | strict | DateValue() |  |
| DAY | 1 | strict | CalendarFieldFunction.DAY |  |
| DAYS | varies | strict | Days.instance | ATP |
| DAYS360 | 2-3 | strict | Days360() |  |
| EDATE | varies | strict | EDate.instance | ATP |
| EOMONTH | varies | strict | EOMonth.instance | ATP |
| HOUR | 1 | strict | CalendarFieldFunction.HOUR |  |
| MINUTE | 1 | strict | CalendarFieldFunction.MINUTE |  |
| MONTH | 1 | strict | CalendarFieldFunction.MONTH |  |
| NETWORKDAYS | varies | strict | NetworkdaysFunction.instance | ATP |
| NOW | 0 | strict | Now | volatile |
| SECOND | 1 | strict | CalendarFieldFunction.SECOND |  |
| TIME | 3 | strict | TimeFunc() |  |
| TIMEVALUE | 1 | strict | TimeValue() |  |
| TODAY | 0 | strict | Today | volatile |
| WEEKDAY | 1-2 | strict | WeekdayFunc.instance |  |
| WEEKNUM | varies | strict | WeekNum.instance | ATP |
| WORKDAY | varies | strict | WorkdayFunction.instance | ATP |
| WORKDAY.INTL | varies | strict | WorkdayIntlFunction.instance | ATP |
| YEAR | 1 | strict | CalendarFieldFunction.YEAR |  |
| YEARFRAC | varies | strict | YearFrac.instance | ATP |

## Statistics (99)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| AVERAGE | 1-30 | strict | AggregateFunction.AVERAGE |  |
| AVERAGEA | 1-30 | strict | AggregateFunction.AVERAGEA |  |
| AVERAGEIF | varies | strict | AverageIf.instance | ATP |
| AVERAGEIFS | varies | strict | Averageifs.instance | ATP |
| BETADIST | 3-5 | strict | NotImplementedFunction |  |
| BETAINV | 3-5 | strict | NotImplementedFunction |  |
| BINOMDIST | 4 | strict | NotImplementedFunction |  |
| CHIDIST | 2 | strict | NotImplementedFunction |  |
| CHIINV | 2 | strict | NotImplementedFunction |  |
| CHITEST | 2 | strict | NotImplementedFunction |  |
| CONFIDENCE | 3 | strict | NotImplementedFunction |  |
| CORREL | 2 | strict | Correl.instance |  |
| COUNT | 0-30 | strict | Count() |  |
| COUNTA | 0-30 | strict | Counta() |  |
| COUNTBLANK | 1 | strict | Countblank() |  |
| COUNTIF | 2 | strict | Countif() |  |
| COUNTIFS | varies | strict | Countifs.instance | ATP |
| COVAR | 2 | strict | Covar.instanceP |  |
| COVARIANCE.P | varies | strict | Covar.instanceP | ATP |
| COVARIANCE.S | varies | strict | Covar.instanceS | ATP |
| CRITBINOM | 3 | strict | NotImplementedFunction |  |
| DEVSQ | 1-30 | strict | AggregateFunction.DEVSQ |  |
| EXPONDIST | 3 | strict | NotImplementedFunction |  |
| FDIST | 3 | strict | NotImplementedFunction |  |
| FINV | 3 | strict | NotImplementedFunction |  |
| FISHER | 1 | strict | NotImplementedFunction |  |
| FISHERINV | 1 | strict | NotImplementedFunction |  |
| FORECAST | 3 | strict | Forecast.instance |  |
| FORECAST.LINEAR | varies | strict | Forecast.instance | ATP |
| FTEST | 2 | strict | NotImplementedFunction |  |
| GAMMADIST | 4 | strict | NotImplementedFunction |  |
| GAMMAINV | 3 | strict | NotImplementedFunction |  |
| GAMMALN | 1 | strict | NotImplementedFunction |  |
| GEOMEAN | 1-30 | strict | AggregateFunction.GEOMEAN |  |
| GROWTH | 1-4 | strict | NotImplementedFunction | array-aware |
| HARMEAN | 1-30 | strict | NotImplementedFunction |  |
| HYPGEOMDIST | 4 | strict | NotImplementedFunction |  |
| INTERCEPT | 2 | strict | Intercept() |  |
| KURT | 1-30 | strict | NotImplementedFunction |  |
| LARGE | 2 | strict | AggregateFunction.LARGE |  |
| LINEST | 1-4 | strict | NotImplementedFunction | array-aware |
| LOGEST | 1-4 | strict | NotImplementedFunction | array-aware |
| LOGINV | 3 | strict | NotImplementedFunction |  |
| LOGNORMDIST | 3 | strict | NotImplementedFunction |  |
| MAX | 1-30 | strict | AggregateFunction.MAX |  |
| MAXA | 1-30 | strict | MinaMaxa.MAXA |  |
| MAXIFS | varies | strict | Maxifs.instance | ATP |
| MEDIAN | 1-30 | strict | AggregateFunction.MEDIAN |  |
| MIN | 1-30 | strict | AggregateFunction.MIN |  |
| MINA | 1-30 | strict | MinaMaxa.MINA |  |
| MINIFS | varies | strict | Minifs.instance | ATP |
| MODE | 1-30 | strict | Mode() | array-aware |
| NEGBINOMDIST | 3 | strict | NotImplementedFunction |  |
| NORM.DIST | varies | strict | NormDist.instance | ATP |
| NORM.INV | varies | strict | NormInv.instance | ATP |
| NORM.S.DIST | varies | strict | NormSDist.instance | ATP |
| NORM.S.INV | varies | strict | NormSInv.instance | ATP |
| NORMDIST | 4 | strict | NormDist.instance |  |
| NORMINV | 3 | strict | NormInv.instance |  |
| NORMSDIST | 1 | strict | NormSDist.instance |  |
| NORMSINV | 1 | strict | NormSInv.instance |  |
| PEARSON | 2 | strict | Correl.instance |  |
| PERCENTILE | 2 | strict | AggregateFunction.PERCENTILE |  |
| PERCENTRANK | 2-3 | strict | PercentRank.instance |  |
| PERCENTRANK.EXC | varies | strict | PercentRankExcFunction.instance | ATP |
| PERCENTRANK.INC | varies | strict | PercentRankIncFunction.instance | ATP |
| POISSON | 3 | strict | NumericFunction.POISSON |  |
| POISSON.DIST | varies | strict | Poisson.instance | ATP |
| PROB | 3-4 | strict | NotImplementedFunction |  |
| QUARTILE | 2 | strict | NotImplementedFunction |  |
| RANK | 2-3 | strict | Rank() |  |
| RSQ | 2 | strict | NotImplementedFunction |  |
| SKEW | 1-30 | strict | NotImplementedFunction |  |
| SLOPE | 2 | strict | Slope() |  |
| SMALL | 2 | strict | AggregateFunction.SMALL |  |
| STANDARDIZE | 3 | strict | Standardize.instance |  |
| STDEV | 1-30 | strict | AggregateFunction.STDEV |  |
| STDEV.P | varies | strict | Stdevp.instance | ATP |
| STDEV.S | varies | strict | Stdevs.instance | ATP |
| STDEVA | 1-30 | strict | AggregateFunction.STDEVA |  |
| STDEVP | 1-30 | strict | AggregateFunction.STDEVP |  |
| STDEVPA | 1-30 | strict | AggregateFunction.STDEVPA |  |
| STEYX | 2 | strict | NotImplementedFunction |  |
| T.DIST | varies | strict | TDistLt.instance | ATP |
| T.DIST.2T | varies | strict | TDist2t.instance | ATP |
| T.DIST.RT | varies | strict | TDistRt.instance | ATP |
| TDIST | 3 | strict | TDist.instance |  |
| TINV | 2 | strict | NotImplementedFunction |  |
| TREND | 1-4 | strict | Trend() | array-aware |
| TRIMMEAN | 2 | strict | NotImplementedFunction |  |
| TTEST | 4 | strict | NotImplementedFunction |  |
| VAR | 1-30 | strict | AggregateFunction.VAR |  |
| VAR.P | varies | strict | Varp.instance | ATP |
| VAR.S | varies | strict | Vars.instance | ATP |
| VARA | 1-30 | strict | AggregateFunction.VARA |  |
| VARP | 1-30 | strict | AggregateFunction.VARP |  |
| VARPA | 1-30 | strict | AggregateFunction.VARPA |  |
| WEIBULL | 4 | strict | NotImplementedFunction |  |
| ZTEST | 2-3 | strict | NotImplementedFunction |  |

## Financial (46)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| ACCRINT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| ACCRINTM | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| AMORDEGRC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| AMORLINC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COUPDAYBS | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COUPDAYS | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COUPDAYSNC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COUPNCD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COUPNUM | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COUPPCD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUMIPMT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUMPRINC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| DB | 4-5 | strict | NotImplementedFunction |  |
| DDB | 4-5 | strict | NotImplementedFunction |  |
| DISC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| DURATION | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| EFFECT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| FV | 3-5 | strict | FinanceFunction.FV |  |
| INTRATE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IPMT | 4-6 | strict | IPMT() |  |
| IRR | 1-2 | strict | Irr() |  |
| ISPMT | 4 | strict | NotImplementedFunction |  |
| MDURATION | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| MIRR | 3 | strict | Mirr() |  |
| NOMINAL | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| NPER | 3-5 | strict | FinanceFunction.NPER |  |
| NPV | 2-30 | strict | Npv() |  |
| PMT | 3-5 | strict | FinanceFunction.PMT |  |
| PPMT | 4-6 | strict | PPMT() |  |
| PRICE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| PRICEDISC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| PRICEMAT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| PV | 3-5 | strict | FinanceFunction.PV |  |
| RATE | 3-6 | strict | Rate() |  |
| RECEIVED | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| SLN | 3 | strict | NotImplementedFunction |  |
| SYD | 4 | strict | NotImplementedFunction |  |
| TBILLEQ | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| TBILLPRICE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| TBILLYIELD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| VDB | 5-7 | strict | NotImplementedFunction |  |
| XIRR | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| XNPV | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| YIELD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| YIELDDISC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| YIELDMAT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |

## Information (24)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| AREAS | 1 | strict | Areas() |  |
| CELL | 1-2 | strict | NotImplementedFunction | volatile |
| COLUMN | 0-1 | strict | Column |  |
| COLUMNS | 1 | strict | Columns() |  |
| ERROR | 0-2 | strict | NotImplementedFunction |  |
| ERROR.TYPE | 1 | strict | Errortype() |  |
| INFO | 1 | strict | NotImplementedFunction | volatile |
| ISBLANK | 1 | strict | LogicalFunction.ISBLANK |  |
| ISERR | 1 | strict | LogicalFunction.ISERR |  |
| ISERROR | 1 | strict | LogicalFunction.ISERROR |  |
| ISEVEN | varies | strict | ParityFunction.IS_EVEN | ATP |
| ISLOGICAL | 1 | strict | LogicalFunction.ISLOGICAL |  |
| ISNA | 1 | strict | LogicalFunction.ISNA |  |
| ISNONTEXT | 1 | strict | LogicalFunction.ISNONTEXT |  |
| ISNUMBER | 1 | strict | LogicalFunction.ISNUMBER |  |
| ISODD | varies | strict | ParityFunction.IS_ODD | ATP |
| ISREF | 1 | strict | LogicalFunction.ISREF |  |
| ISTEXT | 1 | strict | LogicalFunction.ISTEXT |  |
| NA | 0 | strict | Na |  |
| ROW | 0-1 | strict | RowFunc |  |
| ROWS | 1 | strict | Rows() |  |
| SHEET | varies | strict | Sheet.instance | ATP |
| SINGLE | varies | strict | Single.instance | ATP |
| TYPE | 1 | strict | NotImplementedFunction |  |

## Database (12)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| DAVERAGE | 3 | strict | DStarRunner |  |
| DCOUNT | 3 | strict | DStarRunner |  |
| DCOUNTA | 3 | strict | DStarRunner |  |
| DGET | 3 | strict | DStarRunner |  |
| DMAX | 3 | strict | DStarRunner |  |
| DMIN | 3 | strict | DStarRunner |  |
| DPRODUCT | 3 | strict | DStarRunner |  |
| DSTDEV | 3 | strict | DStarRunner |  |
| DSTDEVP | 3 | strict | DStarRunner |  |
| DSUM | 3 | strict | DStarRunner |  |
| DVAR | 3 | strict | DStarRunner |  |
| DVARP | 3 | strict | DStarRunner |  |

## Engineering (39)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| BESSELI | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| BESSELJ | varies | strict | BesselJ.instance | ATP |
| BESSELK | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| BESSELY | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| BIN2DEC | varies | strict | Bin2Dec.instance | ATP |
| BIN2HEX | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| BIN2OCT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| COMPLEX | varies | strict | Complex.instance | ATP |
| CONVERT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| DEC2BIN | varies | strict | Dec2Bin.instance | ATP |
| DEC2HEX | varies | strict | Dec2Hex.instance | ATP |
| DEC2OCT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| DELTA | varies | strict | Delta.instance | ATP |
| ERF | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| ERFC | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| GESTEP | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| HEX2BIN | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| HEX2DEC | varies | strict | Hex2Dec.instance | ATP |
| HEX2OCT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMABS | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMAGINARY | varies | strict | Imaginary.instance | ATP |
| IMARGUMENT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMCONJUGATE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMCOS | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMDIV | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMEXP | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMLN | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMLOG10 | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMLOG2 | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMPOWER | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMPRODUCT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMREAL | varies | strict | ImReal.instance | ATP |
| IMSIN | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMSQRT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMSUB | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| IMSUM | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| OCT2BIN | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| OCT2DEC | varies | strict | Oct2Dec.instance | ATP |
| OCT2HEX | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |

## Miscellaneous (49)
| Name | Arity | Eval | Impl class | Notes |
|------|-------|------|------------|-------|
| ABSREF | 2 | strict | NotImplementedFunction |  |
| ADDRESS | 2-5 | strict | Address() |  |
| APP.TITLE | 0-1 | strict | NotImplementedFunction |  |
| ARGUMENT | 0-3 | strict | NotImplementedFunction |  |
| BAHTTEXT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CALL | 1-3 | strict | NotImplementedFunction |  |
| CUBEKPIMEMBER | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUBEMEMBER | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUBEMEMBERPROPERTY | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUBERANKEDMEMBER | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUBESET | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUBESETCOUNT | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| CUBEVALUE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| DATESTRING | 1 | strict | NotImplementedFunction |  |
| DOLLARDE | varies | strict | DollarDe.instance | ATP |
| DOLLARFR | varies | strict | DollarFr.instance | ATP |
| ENABLE.TOOL | 3 | strict | NotImplementedFunction |  |
| END.IF | 0 | strict | NotImplementedFunction |  |
| EVALUATE | 1 | strict | NotImplementedFunction |  |
| EXEC | 1-4 | strict | NotImplementedFunction |  |
| FVSCHEDULE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| GET.CELL | 1-2 | strict | NotImplementedFunction |  |
| GET.DOCUMENT | 1-2 | strict | NotImplementedFunction |  |
| GET.WINDOW | 1-2 | strict | NotImplementedFunction |  |
| GET.WORKBOOK | 1-2 | strict | NotImplementedFunction |  |
| GET.WORKSPACE | 1 | strict | NotImplementedFunction |  |
| GOTO | 1 | strict | NotImplementedFunction |  |
| LAST.ERROR | 0 | strict | NotImplementedFunction |  |
| MULTINOMIAL | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| NUMBERSTRING | 2 | strict | NotImplementedFunction |  |
| ODDFPRICE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| ODDFYIELD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| ODDLPRICE | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| ODDLYIELD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| PHONETIC | 1 | strict | NotImplementedFunction |  |
| PRESS.TOOL | 3 | strict | NotImplementedFunction |  |
| RAND | 0 | strict | NumericFunction.RAND | volatile |
| REGISTER.ID | 2-3 | strict | NotImplementedFunction |  |
| RELREF | 2 | strict | NotImplementedFunction |  |
| RETURN | 1 | strict | NotImplementedFunction |  |
| RTD | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| SAVE.TOOLBAR | 0-2 | strict | NotImplementedFunction |  |
| SERIESSUM | varies | strict | NotImplementedATPFunction | NotImplemented | ATP |
| STEP | 0 | strict | NotImplementedFunction |  |
| SUBTOTAL | 2-30 | strict | Subtotal() |  |
| SUMIF | 2-3 | strict | Sumif() |  |
| SUMIFS | varies | strict | Sumifs.instance | ATP |
| USDOLLAR | 1-2 | strict | NotImplementedFunction |  |
| WINDOW.TITLE | 0-1 | strict | NotImplementedFunction |  |
