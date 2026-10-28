(ns clojure-algorithms.maths.sumset
  "Calculates the SumSet of two sets of numbers (A and B)

  Source:
      https://en.wikipedia.org/wiki/Sumset")

(defn sumset
  "Returns the sumset of `set-a` and `set-b`: all pairwise sums a + b."
  [set-a set-b]
  (assert (set? set-a) (str "The input value of [set_a=" set-a "] is not a set"))
  (assert (set? set-b) (str "The input value of [set_b=" set-b "] is not a set"))
  (set (for [a set-a, b set-b] (+ a b))))
