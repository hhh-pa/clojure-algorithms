(ns clojure-algorithms.maths.special-numbers.evil-number
  "== Evil Number ==
An evil number is a non-negative integer that has an even number of 1s in its
binary expansion. Numbers that are not evil are called odious numbers.

Examples of evil numbers: 0, 3, 5, 6, 9, 10, 12, 15, 17, 18, 20, 23, 24, 27, ...
Reference: https://en.wikipedia.org/wiki/Evil_number")

(defn is-evil-number
  "Returns true if `number` is evil, i.e. its binary expansion contains an
even number of 1s. Throws an ex-info with :type :TypeError when `number` is
not an integer, and :type :ValueError when it is negative."
  [number]
  (when-not (integer? number)
    (throw (ex-info "Input must be an integer" {:type :TypeError})))
  (when (neg? number)
    (throw (ex-info "Input must be a non-negative integer" {:type :ValueError})))
  (loop [temp number, ones 0]
    (if (zero? temp)
      (even? ones)
      (recur (quot temp 2) (if (odd? temp) (inc ones) ones)))))
