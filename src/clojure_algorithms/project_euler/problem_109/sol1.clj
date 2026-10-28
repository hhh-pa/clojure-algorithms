(ns clojure-algorithms.project-euler.problem-109.sol1
  "Darts checkouts.

  In the game of darts a player throws three darts at a target board split
  into twenty equal sized sections. A checkout finishes the game on a double.
  There are exactly eleven distinct ways to checkout on a score of 6, and
  42336 distinct ways of checking out in total. Counts the distinct ways a
  player can checkout with a score less than a given limit.")

(defn solution
  "Counts the number of distinct ways a player can checkout with a score
  less than limit."
  ([] (solution 100))
  ([limit]
   (let [singles (conj (vec (range 1 21)) 25)
         doubles (conj (vec (map #(* 2 %) (range 1 21))) 50)
         triples (vec (map #(* 3 %) (range 1 21)))
         all-values (vec (concat singles doubles triples [0]))
         n (count all-values)]
     (reduce +
             0
             (for [double-score doubles
                   i (range n)
                   j (range i n)
                   :let [total (+ double-score
                                  (nth all-values i)
                                  (nth all-values j))]
                   :when (< total limit)]
               1)))))
