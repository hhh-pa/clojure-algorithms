(ns clojure-algorithms.project-euler.problem-044.sol1
  "Euler Problem 44: find the pair of pentagonal numbers Pj and Pk for which
  their sum and difference are pentagonal and D = |Pk - Pj| is minimised.")

(defn pentagonal?
  "Returns true if n is pentagonal."
  [n]
  (let [root (Math/sqrt (+ 1 (* 24 n)))]
    (zero? (mod (/ (+ 1 root) 6) 1))))

(defn solution
  "Returns the minimum difference of two pentagonal numbers P1 and P2 (with
  indices below limit) such that P1 + P2 and P2 - P1 are pentagonal, or -1 if
  no such pair exists."
  ([] (solution 5000))
  ([limit]
   (let [pents (vec (map (fn [i] (quot (* i (- (* 3 i) 1)) 2))
                         (range 1 limit)))]
     (or (some (fn [i]
                 (let [pi (pents i)]
                   (some (fn [j]
                           (let [pj (pents j)
                                 a (+ pi pj)
                                 b (- pj pi)]
                             (when (and (pentagonal? a) (pentagonal? b)) b)))
                         (range i (count pents)))))
               (range (count pents)))
         -1))))
