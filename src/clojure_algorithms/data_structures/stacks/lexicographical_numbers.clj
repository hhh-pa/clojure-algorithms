(ns clojure-algorithms.data-structures.stacks.lexicographical-numbers
  "Port of data_structures/stacks/lexicographical_numbers.py.
  Lexical-order number generation with an explicit stack.")

(defn lexical-order
  "Lazy seq of 1..MAX-NUMBER in lexical order."
  [max-number]
  (letfn [(step [stack]
            (when (seq stack)
              (let [num (peek stack)
                    stack (pop stack)]
                (if (> num max-number)
                  (step stack)
                  (cons num
                        (lazy-seq
                         (step (cond-> stack
                                 (not= (mod num 10) 9) (conj (inc num))
                                 true (conj (* num 10))))))))))]
    (step [1])))
