(ns clojure-algorithms.maths.continued-fraction
  "Finding the continuous fraction for a rational number using python\n\nhttps://en.wikipedia.org/wiki/Continued_fraction"
  (:require [clojure.string :as str]))

(declare continued-fraction)

(defn continued-fraction ":param num:\nFraction of the number whose continued fractions to be found.\nUse Fraction(str(number)) for more accurate results due to\nfloat inaccuracies.\n\n:return:\nThe continued fraction of rational number.\nIt is the all commas in the (n + 1)-tuple notation.\n\n>>> continued_fraction(Fraction(2))\n[2]\n>>> continued_fraction(Fraction(\"3.245\"))\n[3, 4, 12, 4]\n>>> continued_fraction(Fraction(\"2.25\"))\n[2, 4]\n>>> continued_fraction(1/Fraction(\"2.25\"))\n[0, 2, 4]\n>>> continued_fraction(Fraction(\"415/93\"))\n[4, 2, 6, 7]\n>>> continued_fraction(Fraction(0))\n[0]\n>>> continued_fraction(Fraction(0.75))\n[0, 1, 3]\n>>> continued_fraction(Fraction(\"-2.25\"))    # -2.25 = -3 + 0.75\n[-3, 1, 3]"
  [& _args]
  ;; stub: original requires method .as_integer_ratio; pure-Clojure port not applicable
  (throw (ex-info "requires method .as_integer_ratio" {:type :UnsupportedOperation})))

;; NOTE: continued_fraction stubbed (method .as_integer_ratio)
