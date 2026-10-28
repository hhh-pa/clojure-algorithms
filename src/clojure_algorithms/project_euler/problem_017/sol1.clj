(ns clojure-algorithms.project-euler.problem-017.sol1
  "Project Euler Problem 17: number letter counts.

  If the numbers 1 to 5 are written out in words: one, two, three, four,
  five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written
  out in words, how many letters would be used?

  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
  and forty-two) contains 23 letters and 115 (one hundred and fifteen)
  contains 20 letters. The use of \"and\" when writing out numbers is in
  compliance with British usage.")

(def ^:private ones-counts
  "Letter counts of zero, one, two, ..., nineteen (0 for zero since it is
  never said aloud)."
  [0 3 3 5 4 4 3 5 5 4 3 6 6 8 8 7 7 9 8 8])

(def ^:private tens-counts
  "Letter counts of twenty, thirty, ..., ninety (0 below twenty due to
  the inconsistency in the teens)."
  [0 0 6 6 5 5 5 7 6 6])

(defn solution
  "Returns the number of letters used to write all numbers from 1 to n,
  where n is lower than or equal to 1000."
  ([] (solution 1000))
  ([n]
   (reduce
    (fn [count i]
      (if (< i 1000)
        (let [count (if (>= i 100)
                      ;; Add letters for \"n hundred\", plus \"and\" when i
                      ;; is not a multiple of 100.
                      (+ count (ones-counts (quot i 100)) 7
                         (if (zero? (mod i 100)) 0 3))
                      count)]
          (if (and (pos? (mod i 100)) (< (mod i 100) 20))
            ;; one, two, three, ..., nineteen
            (+ count (ones-counts (mod i 100)))
            ;; twenty, twenty one, ..., ninety nine
            (+ count (ones-counts (mod i 10))
               (tens-counts (quot (- (mod i 100) (mod i 10)) 10)))))
        ;; one thousand
        (+ count (ones-counts (quot i 1000)) 8)))
    0 (range 1 (inc n)))))
