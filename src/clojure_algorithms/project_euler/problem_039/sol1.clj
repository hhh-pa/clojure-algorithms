(ns clojure-algorithms.project-euler.problem-039.sol1
  "Euler Problem 39: for which value of p <= 1000 is the number of integral
  right-triangle solutions with perimeter p maximised?")

(defn- perimeters-in-order
  "Returns the perimeters of all integral right triangles with perimeter up to
  max-perimeter, in the order the base/perpendicular loops encounter them."
  [max-perimeter]
  (vec (for [base (range 1 (inc max-perimeter))
             perp (range base (inc max-perimeter))
             :let [hyp (Math/sqrt (+ (* base base) (* perp perp)))]
             :when (== hyp (long hyp))
             :let [perimeter (long (+ base perp hyp))]
             :when (<= perimeter max-perimeter)]
         perimeter)))

(defn pythagorean-triple
  "Returns a map from the perimeter of an integral right triangle to the number
  of corresponding triplets, for perimeters up to max-perimeter."
  [max-perimeter]
  (frequencies (perimeters-in-order max-perimeter)))

(defn solution
  "Returns the perimeter up to n with the maximum number of solutions. Ties
  are broken by encounter order, like Python's Counter.most_common."
  ([] (solution 1000))
  ([n]
   (let [perims (perimeters-in-order n)
         counts (frequencies perims)
         best (apply max (vals counts))]
     (first (filter (fn [p] (= best (counts p))) perims)))))
