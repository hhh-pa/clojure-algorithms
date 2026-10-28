(ns clojure-algorithms.maths.special-numbers.jacobsthal-number
  "Jacobsthal numbers

The Jacobsthal sequence starts with 0 and 1, and every later term is the
previous term plus twice the term before it:

    J(0) = 0
    J(1) = 1
    J(n) = J(n - 1) + 2 * J(n - 2)

The first terms are 0, 1, 1, 3, 5, 11, 21, 43, 85, 171, 341, ...

They also have the closed form J(n) = (2 ** n - (-1) ** n) / 3, which is used
below to cross-check the iterative result.

Source:
    https://en.wikipedia.org/wiki/Jacobsthal_number
    https://oeis.org/A001045")

(defn- value-repr
  "Python-repr-style rendering used in error messages: strings are wrapped in
single quotes, other values use their plain string form."
  [v]
  (if (string? v) (str "'" v "'") (str v)))

(defn- check-non-negative-int!
  "Throws an ex-info with :type :ValueError unless v is a non-negative integer."
  [name v]
  (when (or (not (integer? v)) (neg? v))
    (throw (ex-info (str name " must be a non-negative integer, got "
                         (value-repr v))
                    {:type :ValueError}))))

(defn jacobsthal-number
  "Returns the nth Jacobsthal number, counting from J(0) = 0. Throws an
ex-info with :type :ValueError when n is not a non-negative integer."
  [n]
  (check-non-negative-int! "n" n)
  (loop [i 0, previous 0, current 1]
    (if (= i n)
      previous
      (recur (inc i) current (+' current (*' 2 previous))))))

(defn jacobsthal-sequence
  "Returns a vector of the first `length` Jacobsthal numbers. Throws an
ex-info with :type :ValueError when `length` is not a non-negative integer."
  [length]
  (check-non-negative-int! "length" length)
  (loop [i 0, previous 0, current 1, acc []]
    (if (= i length)
      acc
      (recur (inc i) current (+' current (*' 2 previous)) (conj acc previous)))))
