(ns user
  (:require [rechentafel.eval :as e]
            [rechentafel.cell :as c]
            [rechentafel.value :as v]
            [rechentafel.functions :as f]
            [rechentafel.functions.all]))

(comment

  ;; Build a workbook programmatically.
  (def wb (-> (e/empty-workbook ["Sheet1"])
              (e/set-cell (c/pack 0 0 0) 10)
              (e/set-cell (c/pack 0 0 1) 20)
              (e/set-cell (c/pack 0 0 2) "=A1+B1")
              (e/recalc)))

  (e/get-cell wb (c/pack 0 0 2))
  ;;=> {:t :num :v 30.0}

  ;; Load an existing XLSX (needs :poi or :dev alias).
  (require 'rechentafel.poi)
  (def wb2 (rechentafel.poi/load-workbook "resources/samples/poi/SampleSS.xlsx"))
  (count (:formulas wb2)))
