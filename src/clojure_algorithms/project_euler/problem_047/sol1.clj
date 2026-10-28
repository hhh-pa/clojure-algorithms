(ns clojure-algorithms.project-euler.problem-047.sol1
  "Euler Problem 47: find the first four consecutive integers to have four
  distinct prime factors each.")

(defn unique-prime-factors
  "Returns the set of distinct prime factors of n."
  [n]
  (loop [n n i 2 factors #{}]
    (cond
      (> (* i i) n) (if (> n 1) (conj factors n) factors)
      (zero? (mod n i)) (recur (quot n i) i (conj factors i))
      :else (recur n (inc i) factors))))

(def upf-len
  "Memoized count of distinct prime factors of num."
  (memoize (fn [num] (count (unique-prime-factors num)))))

(defn equality
  "Returns true if all elements of the collection are equal."
  [coll]
  (contains? #{0 1} (count (set coll))))

(defn run
  "Finds the first n consecutive integers each having n distinct prime
  factors, returned as a vector."
  [n]
  (loop [base 2]
    (let [group (vec (map #(+ base %) (range n)))
          checker (conj (mapv upf-len group) n)]
      (if (equality checker) group (recur (inc base))))))

(defn solution
  "Returns the first of the first n consecutive integers with n distinct prime
  factors each, or nil if there is none."
  ([] (solution 4))
  ([n]
   (let [results (run n)]
     (when (seq results) (first results)))))
