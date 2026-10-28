(ns clojure-algorithms.maths.print-multiplication-table
  "Port of maths/print_multiplication_table.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare multiplication-table)

(defn multiplication-table "Prints the multiplication table of a given number till the given number of terms\n\n>>> print(multiplication_table(3, 5))\n3 * 1 = 3\n3 * 2 = 6\n3 * 3 = 9\n3 * 4 = 12\n3 * 5 = 15\n\n>>> print(multiplication_table(-4, 6))\n-4 * 1 = -4\n-4 * 2 = -8\n-4 * 3 = -12\n-4 * 4 = -16\n-4 * 5 = -20\n-4 * 6 = -24"
  [number number-of-terms] (clojure.string/join "\n" (map py-str (doall (for [i (py-iter (range 1 (py-add number-of-terms 1)))] (str (py-str number) " * " (py-str i) " = " (py-str (py-mul number i))))))))
