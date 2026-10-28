(ns clojure-algorithms.maths.series.hexagonal-numbers
  "A hexagonal number sequence is a sequence of figurate numbers
where the nth hexagonal number hₙ is the number of distinct dots
in a pattern of dots consisting of the outlines of regular
hexagons with sides up to n dots, when the hexagons are overlaid
so that they share one vertex.

    Calculates the hexagonal numbers sequence with a formula
        hₙ = n(2n-1)
        where:
        hₙ --> is nth element of the sequence
        n --> is the number of element in the sequence
        reference-->\"Hexagonal number\" Wikipedia
        <https://en.wikipedia.org/wiki/Hexagonal_number>")

(defn hexagonal-numbers
  "Return the first length hexagonal numbers.

  Throws ex-info with :type :ValueError when length is not a positive
  integer."
  [length]
  (when (or (not (integer? length)) (<= length 0))
    (throw (ex-info "Length must be a positive integer." {:type :ValueError})))
  (mapv (fn [n] (* n (- (* 2 n) 1))) (range length)))
