(ns clojure-algorithms.project-euler.problem-117.sol1
  "Project Euler Problem 117: multi-colour tilings.

  Using a combination of grey square tiles and oblong tiles chosen from red
  tiles (two units), green tiles (three units), and blue tiles (four units),
  it is possible to tile a row measuring five units in length in exactly
  fifteen different ways. Counts the ways a row measuring fifty units in
  length can be tiled.")

(defn solution
  "Returns the number of ways a row of the given length can be tiled."
  ([] (solution 50))
  ([length]
   (let [ways (reduce (fn [acc row-length]
                        (conj acc
                              (+ 1 (reduce + 0
                                           (for [tile-length (range 2 5)
                                                 tile-start (range (- (inc row-length)
                                                                      tile-length))]
                                             (nth acc (- row-length
                                                         tile-start
                                                         tile-length)))))))
                      []
                      (range (inc length)))]
     (nth ways length))))
