(ns clojure-algorithms.maths.special-numbers.triangular-numbers
  "A triangular number or triangle number counts objects arranged in an
equilateral triangle. This module provides a function to generate n'th
triangular number.

For more information about triangular numbers, refer to:
https://en.wikipedia.org/wiki/Triangular_number")

(defn triangular-number
  "Returns the triangular number at `position`: position * (position + 1) / 2.
Throws an ex-info with :type :ValueError when `position` is negative."
  [position]
  (when (neg? position)
    (throw (ex-info "param `position` must be non-negative"
                    {:type :ValueError})))
  (quot (*' position (inc position)) 2))
