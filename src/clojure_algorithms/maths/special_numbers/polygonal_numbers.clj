(ns clojure-algorithms.maths.special-numbers.polygonal-numbers
  "Polygonal numbers generalize triangular and square numbers to regular
polygons with any number of sides.
See https://en.wikipedia.org/wiki/Polygonal_number.")

(defn polygonal-num
  "Returns the `num`th `sides`-gonal number. Requires `num` >= 0 and
`sides` >= 3, otherwise throws an ex-info with :type :ValueError."
  [num sides]
  (when (or (< num 0) (< sides 3))
    (throw (ex-info "Invalid input: num must be >= 0 and sides must be >= 3."
                    {:type :ValueError})))
  (quot (- (*' (- sides 2) (*' num num)) (*' (- sides 4) num)) 2))
