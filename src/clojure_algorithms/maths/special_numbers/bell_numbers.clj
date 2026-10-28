(ns clojure-algorithms.maths.special-numbers.bell-numbers
  "Bell numbers represent the number of ways to partition a set into non-empty
subsets. This module provides functions to calculate Bell numbers for sets of
integers. In other words, the first (n + 1) Bell numbers.

For more information about Bell numbers, refer to:
https://en.wikipedia.org/wiki/Bell_number")

(defn -binomial-coefficient
  "The binomial coefficient C(total-elements, elements-to-choose)."
  [total-elements elements-to-choose]
  (if (or (zero? elements-to-choose) (= elements-to-choose total-elements))
    1
    (let [k (min elements-to-choose (- total-elements elements-to-choose))]
      (reduce (fn [coefficient i]
                (quot (*' coefficient (- total-elements i)) (inc i)))
              1
              (range k)))))

(defn bell-numbers
  "Returns a vector of the Bell numbers for set lengths 0 through
`max-set-length`, i.e. the first (`max-set-length` + 1) Bell numbers.
Throws an ex-info with :type :ValueError when `max-set-length` is negative."
  [max-set-length]
  (when (neg? max-set-length)
    (throw (ex-info "max_set_length must be non-negative" {:type :ValueError})))
  (reduce (fn [bell i]
            (conj bell
                  (reduce +' 0
                          (map-indexed (fn [j bell-j]
                                         (*' (-binomial-coefficient (dec i) j)
                                             bell-j))
                                       bell))))
          [1]
          (range 1 (inc max-set-length))))
