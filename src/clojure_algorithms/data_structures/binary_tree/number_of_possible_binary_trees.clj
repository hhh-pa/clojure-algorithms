(ns clojure-algorithms.data-structures.binary-tree.number-of-possible-binary-trees
  "Port of data_structures/binary_tree/number_of_possible_binary_trees.py.
  Catalan numbers and binary-tree counts.")

(defn binomial-coefficient
  "Binomial coefficient C(n, k)."
  [n k]
  (let [k (min k (- n k))]
    (loop [i 0 result 1]
      (if (>= i k)
        result
        (recur (inc i) (quot (* result (- n i)) (inc i)))))))

(defn catalan-number
  "The NODE-COUNT-th Catalan number."
  [node-count]
  (quot (binomial-coefficient (* 2 node-count) node-count)
        (inc node-count)))

(defn factorial
  "Factorial of N."
  [n]
  (when (neg? n)
    (throw (ex-info "factorial() not defined for negative values"
                    {:type :value-error})))
  (reduce * 1 (range 1 (inc n))))

(defn binary-tree-count
  "Number of binary trees with NODE-COUNT nodes."
  [node-count]
  (* (catalan-number node-count) (factorial node-count)))
