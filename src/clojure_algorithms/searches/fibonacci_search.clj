(ns clojure-algorithms.searches.fibonacci-search
  "Fibonacci search over sorted collections.

  Port of TheAlgorithms/Python searches/fibonacci_search.py.")

(defn- fib-impl
  "Raw recursive Fibonacci with Python-compatible argument checks."
  [k]
  (cond (not (integer? k))
        (throw (ex-info "k must be an integer." {:k k}))
        (neg? k)
        (throw (ex-info "k integer must be greater or equal to zero." {:k k}))
        (zero? k) 0
        (= k 1) 1
        :else (+ (fib-impl (dec k)) (fib-impl (- k 2)))))

(def fibonacci
  "Fibonacci number at index `k` (memoized). Throws when `k` is not a
  non-negative integer."
  (memoize fib-impl))

(defn fibonacci-search
  "Index of `val` in sorted `arr`, or -1 if absent."
  [arr val]
  (let [a (vec arr)
        n (count a)]
    (if (zero? n)
      -1
      (let [fibb-k (loop [i 0]
                     (if (>= (fibonacci i) n) i (recur (inc i))))]
        (loop [offset 0 k fibb-k]
          (if (<= k 0)
            -1
            (let [idx (min (+ offset (fibonacci (dec k))) (dec n))
                  c (compare val (nth a idx))]
              (cond (zero? c) idx
                    (neg? c) (recur offset (dec k))
                    :else (recur (+ offset (fibonacci (dec k))) (- k 2))))))))))
