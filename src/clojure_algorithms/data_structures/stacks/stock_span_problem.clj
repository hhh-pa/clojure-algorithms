(ns clojure-algorithms.data-structures.stacks.stock-span-problem
  "Port of data_structures/stacks/stock_span_problem.py.")

(defn calculate-span
  "Stock span for each price in PRICE."
  [price]
  (let [n (count price)]
    (loop [i 1 stack [0] result (assoc (vec (repeat n 0)) 0 1)]
      (if (>= i n)
        result
        (let [stack (loop [stack stack]
                      (if (and (seq stack) (<= (nth price (peek stack))
                                               (nth price i)))
                        (recur (pop stack))
                        stack))
              result (assoc result i (if (empty? stack)
                                       (inc i)
                                       (- i (peek stack))))]
          (recur (inc i) (conj stack i) result))))))
