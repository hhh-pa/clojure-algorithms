(ns clojure-algorithms.project-euler.problem-075.sol1
  "Project Euler 75: singular integer right triangles.

  Given that L is the length of the wire, for how many values of
  L <= 1,500,000 can exactly one integer sided right angle triangle be
  formed? All Pythagorean triples are generated using Euclid's formula
  while tracking the frequencies of the perimeters.")

(defn- gcd
  "Returns the greatest common divisor of a and b."
  [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn solution
  "Returns the number of values of L <= limit such that a wire of length
  L can be formed into an integer sided right angle triangle in exactly
  one way."
  ([] (solution 1500000))
  ([limit]
   (let [frequencies (int-array (inc limit))]
     (loop [euclid-m 2]
       (when (<= (* 2 euclid-m (inc euclid-m)) limit)
         (doseq [euclid-n (range (inc (mod euclid-m 2)) euclid-m 2)
                 :when (= 1 (gcd euclid-m euclid-n))]
           (let [primitive-perimeter (* 2 euclid-m (+ euclid-m euclid-n))]
             (doseq [perimeter (range primitive-perimeter (inc limit)
                                      primitive-perimeter)]
               (aset frequencies perimeter
                     (inc (aget frequencies perimeter))))))
         (recur (inc euclid-m))))
     (count (filter #(= 1 %) frequencies)))))
