(ns clojure-algorithms.project-euler.problem-135.sol1
  "Project Euler Problem 135: arithmetic progression differences.

  Given positive integers x, y, z as consecutive terms of an arithmetic
  progression, the least n for which x^2 - y^2 - z^2 = n has exactly two
  solutions is n = 27. Taking x, y, z of the form a + d, a, a - d reduces
  the equation to a * (4d - a) = n. Counts how many values of n below one
  million have exactly ten distinct solutions.")

(defn solution
  "Returns the number of values of n less than or equal to limit that have
  exactly ten distinct solutions."
  ([] (solution 1000000))
  ([limit]
   (let [lim (inc limit)
         frequency (long-array lim 0)]
     (doseq [first-term (range 1 lim)
             n (range first-term lim first-term)]
       (let [common-difference (+ first-term (quot n first-term))]
         (when (zero? (mod common-difference 4))
           (let [d (quot common-difference 4)]
             (when (and (> first-term d) (< first-term (* 4 d)))
               (aset frequency n (inc (aget frequency n))))))))
     (count (filter #(= 10 %) frequency)))))
