(ns clojure-algorithms.financial.price-plus-tax)

(defn price-plus-tax
  "Price of a good or service including tax."
  [price tax-rate]
  (* price (+ 1 tax-rate)))
