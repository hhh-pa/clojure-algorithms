(ns clojure-algorithms.maths.sum-of-digits
  "Port of maths/sum_of_digits.py")

(defn sum-of-digits
  "Find the sum of digits of a number."
  [n]
  (loop [n (if (neg? n) (-' n) n), res 0]
    (if (zero? n)
      res
      (recur (quot n 10) (+' res (rem n 10))))))

(defn sum-of-digits-recursion
  "Find the sum of digits of a number using recursion."
  [n]
  (let [n (if (neg? n) (-' n) n)]
    (if (< n 10)
      n
      (+' (rem n 10) (sum-of-digits-recursion (quot n 10))))))

(defn sum-of-digits-compact
  "Find the sum of digits of a number."
  [n]
  (reduce +' 0 (map #(- (int %) (int \0)) (str (if (neg? n) (-' n) n)))))

(defn- benchmark-a-function
  "Times a single call of func on value and prints the result."
  [func value]
  (let [start (System/nanoTime)
        result (func value)
        elapsed (/ (double (- (System/nanoTime) start)) 1e9)]
    (println (format "%-56s = %s -- %.4f seconds"
                     (str func "(" value ")") result elapsed))))

(defn benchmark
  "Benchmark the sum-of-digits functions with three different length ints."
  [& _args]
  (doseq [value [262144
                 1125899906842624
                 1267650600228229401496703205376N]]
    (doseq [func [sum-of-digits sum-of-digits-recursion sum-of-digits-compact]]
      (benchmark-a-function func value))
    (println)))
