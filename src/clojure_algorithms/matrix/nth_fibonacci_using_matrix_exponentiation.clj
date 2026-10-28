(ns clojure-algorithms.matrix.nth-fibonacci-using-matrix-exponentiation
  "Nth Fibonacci number via 2x2 matrix exponentiation (O(log n) matrix
  multiplications) and via the linear brute-force iteration.")

(defn multiply
  "Multiplies two square matrices."
  [matrix-a matrix-b]
  (let [n (count matrix-a)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce +' 0
                            (map (fn [k]
                                   (*' (get-in matrix-a [i k])
                                       (get-in matrix-b [k j])))
                                 (range n))))
                  (range n)))
          (range n))))

(defn identity
  "Returns the n x n identity matrix."
  [n]
  (mapv (fn [r] (mapv (fn [c] (if (= r c) 1 0)) (range n))) (range n)))

(defn nth-fibonacci-matrix
  "Returns the nth Fibonacci number using matrix exponentiation."
  [n]
  (if (<= n 1)
    n
    (loop [result (identity 2)
           fib [[1 1] [1 0]]
           e (dec n)]
      (if (pos? e)
        (recur (if (odd? e) (multiply result fib) result)
               (multiply fib fib)
               (quot e 2))
        (get-in result [0 0])))))

(defn nth-fibonacci-bruteforce
  "Returns the nth Fibonacci number by linear iteration."
  [n]
  (if (<= n 1)
    n
    (loop [fib0 0 fib1 1 i 2]
      (if (> i n)
        fib1
        (recur fib1 (+' fib0 fib1) (inc i))))))

(defn main
  "Prints selected Fibonacci numbers computed both ways."
  []
  (doseq [ordinal ["0th" "1st" "2nd" "3rd" "10th" "100th" "1000th"]]
    (let [n (Long/parseLong (apply str (filter #(Character/isDigit ^char %)
                                               ordinal)))]
      (println (str ordinal " fibonacci number using matrix exponentiation is "
                    (nth-fibonacci-matrix n) " and using bruteforce is "
                    (nth-fibonacci-bruteforce n) "\n")))))
