(ns clojure-algorithms.project-euler.problem-043.sol1
  "Euler Problem 43: find the sum of all 0 to 9 pandigital numbers with the
  sub-string divisibility property (d2d3d4 divisible by 2, d3d4d5 by 3, d4d5d6
  by 5, d5d6d7 by 7, d6d7d8 by 11, d7d8d9 by 13, d8d9d10 by 17).")

(defn substring-divisible?
  "Returns true if the pandigital digit vector passes all divisibility tests."
  [num]
  (let [v (vec num)]
    (and (zero? (mod (v 3) 2))
         (zero? (mod (+ (v 2) (v 3) (v 4)) 3))
         (zero? (mod (v 5) 5))
         (every? true?
                 (map (fn [i test]
                        (zero? (mod (+ (* (v (+ i 4)) 100)
                                       (* (v (+ i 5)) 10)
                                       (v (+ i 6)))
                                    test)))
                      (range)
                      [7 11 13 17])))))

(defn- permutations
  "Returns a lazy sequence of all permutations of coll as vectors."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      (list v)
      (for [i (range (count v))
            rest-p (permutations (into (subvec v 0 i) (subvec v (inc i))))]
        (into [(v i)] rest-p)))))

(defn solution
  "Returns the sum of all 0 to (n-1) pandigital numbers passing the
  divisibility tests."
  ([] (solution 10))
  ([n]
   (reduce + 0 (comp (filter substring-divisible?)
                     (map (fn [num] (Long/parseLong (apply str num)))))
           (permutations (range n)))))
