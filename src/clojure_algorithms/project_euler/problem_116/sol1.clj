(ns clojure-algorithms.project-euler.problem-116.sol1
  "Project Euler Problem 116: single-colour tile replacements.

  A row of grey square tiles has some tiles replaced with coloured oblong
  tiles chosen from red (length two), green (length three), or blue (length
  four). Assuming colours cannot be mixed there are 12 ways of replacing the
  grey tiles in a row measuring five units in length. Counts the ways for a
  row measuring fifty units in length with at least one coloured tile used.")

(defn solution
  "Returns the number of different ways the grey tiles in a row of the given
  length can be replaced if colours cannot be mixed and at least one coloured
  tile must be used."
  ([] (solution 50))
  ([length]
   (let [rows (reduce (fn [acc row-length]
                        (conj acc
                              (vec (for [tile-length (range 2 5)]
                                     (reduce + 0
                                             (for [tile-start (range (- (inc row-length)
                                                                        tile-length))]
                                               (inc (get-in acc [(- row-length
                                                                    tile-start
                                                                    tile-length)
                                                                 (- tile-length 2)]))))))))
                      []
                      (range (inc length)))]
     (reduce + 0 (nth rows length)))))
