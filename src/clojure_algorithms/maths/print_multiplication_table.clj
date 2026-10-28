(ns clojure-algorithms.maths.print-multiplication-table
  "Port of maths/print_multiplication_table.py"
  (:require [clojure.string :as str]))

(defn multiplication-table
  "Returns the multiplication table of number up to number-of-terms
as a newline-separated string."
  [number number-of-terms]
  (str/join "\n"
            (for [i (range 1 (inc number-of-terms))]
              (str number " * " i " = " (* number i)))))
