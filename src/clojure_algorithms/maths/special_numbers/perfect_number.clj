(ns clojure-algorithms.maths.special-numbers.perfect-number
  "== Perfect Number ==
In number theory, a perfect number is a positive integer that is equal to the sum of
its positive divisors, excluding the number itself.
For example: 6 ==> divisors[1, 2, 3, 6]
    Excluding 6, the sum(divisors) is 1 + 2 + 3 = 6
    So, 6 is a Perfect Number

Other examples of Perfect Numbers: 28, 486, ...

https://en.wikipedia.org/wiki/Perfect_number")

(defn perfect
  "Returns true if `number` is perfect: it equals the sum of its proper
divisors. Returns false for integers below 1. Throws an ex-info with :type
:ValueError when `number` is not an integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info "number must be an integer" {:type :ValueError})))
  (if (<= number 0)
    false
    (= number
       (reduce + 0 (filter #(zero? (rem number %))
                           (range 1 (inc (quot number 2))))))))
