(ns clojure-algorithms.project-euler.problem-062.sol1
  "Project Euler 62: cubic permutations.

  The cube, 41063625 (345^3), can be permuted to produce two other cubes:
  56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest
  cube which has exactly three permutations of its digits which are also
  cube.

  Find the smallest cube for which exactly five permutations of its digits
  are cube.")

(defn get-digits
  "Computes the sorted sequence of digits of the cube of num."
  [num]
  (apply str (sort (str (* num num num)))))

(defn solution
  "Iterates through every possible cube and sorts the cube's digits in
  ascending order. Sorting maintains an ordering of the digits that allows
  comparing permutations. Each sorted sequence of digits is stored in a map,
  whose key is the sequence of digits and value is a vector of numbers that
  are the base of the cube. Once max-base numbers produce the same sequence
  of digits, returns the smallest cube."
  ([] (solution 5))
  ([max-base]
   (loop [num 0 freqs {}]
     (let [digits (get-digits num)
           bases (conj (get freqs digits []) num)]
       (if (= (count bases) max-base)
         (let [b (first bases)] (* b b b))
         (recur (inc num) (assoc freqs digits bases)))))))
