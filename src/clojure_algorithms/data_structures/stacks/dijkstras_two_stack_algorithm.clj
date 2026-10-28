(ns clojure-algorithms.data-structures.stacks.dijkstras-two-stack-algorithm
  "Port of data_structures/stacks/dijkstras_two_stack_algorithm.py.
  Evaluates fully-parenthesized single-digit expressions.")

(def operators
  {"*" * "/" (fn [x y] (/ (double x) (double y))) "+" + "-" -})

(defn dijkstras-two-stack-algorithm
  "Evaluate EQUATION with Dijkstra's two-stack algorithm."
  [equation]
  (let [[operands _]
        (reduce (fn [[operands op-stack] ch]
                  (cond
                    (Character/isDigit ^char ch)
                    [(conj operands (Character/digit ch 10)) op-stack]
                    (contains? operators (str ch))
                    [operands (conj op-stack ch)]
                    (= ch \))
                    (let [opr (peek op-stack)
                          op-stack (pop op-stack)
                          num1 (peek operands)
                          operands (pop operands)
                          num2 (peek operands)
                          operands (pop operands)]
                      [(conj operands ((get operators (str opr)) num2 num1))
                       op-stack])
                    :else [operands op-stack]))
                [[] []] equation)]
    (peek operands)))
