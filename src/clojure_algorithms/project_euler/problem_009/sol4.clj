(ns clojure-algorithms.project-euler.problem-009.sol4
  "Project Euler Problem 9: special Pythagorean triplet.

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for
  which a^2 + b^2 = c^2. For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product a*b*c.

  This solution precomputes squares and checks whether a^2 + b^2 is a
  square by set lookup.")

(defn get-squares
  "Returns a vector of the squares of the numbers from 0 up to (but not
  including) n."
  [n]
  (mapv #(* % %) (range n)))

(defn solution
  "Returns the product of a, b, c which form a Pythagorean triplet with
  a + b + c = n, or -1 when there is none."
  ([] (solution 1000))
  ([n]
   (let [squares (get-squares n)
         squares-set (set squares)]
     (or (some (fn [[a b]]
                 (let [c (- n a b)]
                   (when (and (contains? squares-set (+ (squares a) (squares b)))
                              (= (squares c) (+ (squares a) (squares b))))
                     (* a b c))))
               (for [a (range 1 (quot n 3))
                     b (range (inc a) (inc (quot (- n a) 2)))]
                 [a b]))
         -1))))
