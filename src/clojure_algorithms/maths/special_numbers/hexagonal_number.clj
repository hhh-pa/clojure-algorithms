(ns clojure-algorithms.maths.special-numbers.hexagonal-number
  "== Hexagonal Number ==
The nth hexagonal number hn is the number of distinct dots
in a pattern of dots consisting of the outlines of regular
hexagons with sides up to n dots, when the hexagons are
overlaid so that they share one vertex.

https://en.wikipedia.org/wiki/Hexagonal_number")

(defn hexagonal
  "Returns the nth hexagonal number, n * (2n - 1). Only defined for positive
integers. Throws an ex-info with :type :TypeError when `number` is not an
integer, and :type :ValueError when it is below 1."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (when (< number 1)
    (throw (ex-info "Input must be a positive integer" {:type :ValueError})))
  (*' number (dec (*' 2 number))))
