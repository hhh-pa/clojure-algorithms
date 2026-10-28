(ns clojure-algorithms.project-euler.problem-032.sol32
  "Euler Problem 32: find the sum of all products whose
  multiplicand/multiplier/product identity is 1 through 9 pandigital.")

(defn- permutations
  "Returns a lazy sequence of all permutations of coll as vectors."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      (list v)
      (for [i (range (count v))
            rest-p (permutations (into (subvec v 0 i) (subvec v (inc i))))]
        (into [(v i)] rest-p)))))

(defn- digits->num
  "Reads consecutive characters of a permutation as one integer."
  [comb from to]
  (Integer/parseInt (apply str (subvec (vec comb) from to))))

(defn combination-valid?
  "Checks whether a 9-digit combination (a permutation of 1-9) forms a valid
  product equation, either as a 2-digit by 3-digit or a 1-digit by 4-digit
  multiplication."
  [combination]
  (let [c (vec combination)]
    (or (= (* (digits->num c 0 2) (digits->num c 2 5)) (digits->num c 5 9))
        (= (* (digits->num c 0 1) (digits->num c 1 5)) (digits->num c 5 9)))))

(defn solution
  "Finds the sum of all products whose multiplicand/multiplier/product identity
  can be written as a 1 through 9 pandigital."
  []
  (reduce + (into #{}
                  (comp (filter combination-valid?)
                        (map #(digits->num (vec %) 5 9)))
                  (permutations "123456789"))))
