(ns rechentafel.functions.all
  "Aggregator — requiring this namespace loads every fn module so the
  registry is fully populated. Callers that just want `f/call` should
  `(require 'rechentafel.functions.all)` once at startup."
  (:require [rechentafel.fn.math]
            [rechentafel.fn.text]
            [rechentafel.fn.logical]
            [rechentafel.fn.datetime]
            [rechentafel.fn.info]
            [rechentafel.fn.lookup]
            [rechentafel.fn.database]
            [rechentafel.fn.engineering]
            [rechentafel.fn.stats]
            [rechentafel.fn.financial]
            [rechentafel.fn.misc]
            [rechentafel.fn.array]))
