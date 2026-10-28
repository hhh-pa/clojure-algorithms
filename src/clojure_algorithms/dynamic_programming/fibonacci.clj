(ns clojure-algorithms.dynamic-programming.fibonacci
  "Fibonacci sequence via dynamic programming: `fibonacci-seq` returns
  the first `n` numbers. (The Python original wraps this in a stateful
  class; the pure function is equivalent.)
  Ported from TheAlgorithms/Python dynamic_programming/fibonacci.py.")

(defn fibonacci-seq
  "Vector of the first `n` Fibonacci numbers."
  [n]
  (cond
    (<= n 0) []
    (= n 1) [0]
    :else (loop [i 2 s [0 1]]
            (if (>= i n)
              s
              (recur (inc i) (conj s (+ (peek s) (nth s (- (count s) 2)))))))))
