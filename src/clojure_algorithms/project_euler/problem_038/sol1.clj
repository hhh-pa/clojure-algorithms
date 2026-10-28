(ns clojure-algorithms.project-euler.problem-038.sol1
  "Euler Problem 38: find the largest 1 to 9 pandigital 9-digit number that can
  be formed as the concatenated product of an integer with (1,2,...,n), n > 1.")

(defn pandigital-9?
  "Checks whether n is a 9-digit 1 to 9 pandigital number."
  [n]
  (let [s (str n)]
    (and (= (count s) 9)
         (= (set s) (set "123456789")))))

(defn solution
  "Returns the largest 1 to 9 pandigital concatenated product, or nil if none
  exists."
  []
  (or (some (fn [base]
              (let [candidate (* 100002 base)]
                (when (pandigital-9? candidate) candidate)))
            (range 9999 4999 -1))
      (some (fn [base]
              (let [candidate (* 1002003 base)]
                (when (pandigital-9? candidate) candidate)))
            (range 333 99 -1))))
