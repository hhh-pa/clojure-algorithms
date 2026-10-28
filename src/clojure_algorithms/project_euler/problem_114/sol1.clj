(ns clojure-algorithms.project-euler.problem-114.sol1
  "Project Euler Problem 114: red blocks in a row.

  A row measuring seven units in length has red blocks with a minimum length
  of three units placed on it, such that any two red blocks are separated by
  at least one grey square. There are exactly seventeen ways of doing this.
  Counts how many ways a row measuring fifty units in length can be filled.")

(defn solution
  "Returns the number of ways a row of the given length can be filled."
  ([] (solution 50))
  ([length]
   (let [ways (long-array (inc length) 1)]
     (doseq [row-length (range 3 (inc length))]
       (doseq [block-length (range 3 (inc row-length))]
         (doseq [block-start (range (- row-length block-length))]
           (aset ways row-length
                 (+ (aget ways row-length)
                    (aget ways (- row-length block-start block-length 1)))))
         (aset ways row-length (inc (aget ways row-length)))))
     (aget ways length))))
