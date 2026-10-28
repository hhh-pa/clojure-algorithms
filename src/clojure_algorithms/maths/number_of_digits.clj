(ns clojure-algorithms.maths.number-of-digits
  "Port of maths/number_of_digits.py.")

(defn- check-int
  [n]
  (when-not (integer? n)
    (throw (ex-info "Input must be an integer" {:type :TypeError}))))

(defn num-digits
  "Finds the number of digits in a number."
  [n]
  (check-int n)
  (loop [n (abs (long n)) digits 0]
    (let [digits (inc digits)
          n (quot n 10)]
      (if (zero? n)
        digits
        (recur n digits)))))

(defn num-digits-fast
  "Finds the number of digits in a number using logarithms.
  abs() is used as logarithm for negative numbers is not defined."
  [n]
  (check-int n)
  (if (zero? n)
    1
    (int (Math/floor (+ (Math/log10 (Math/abs (double n))) 1)))))

(defn num-digits-faster
  "Finds the number of digits in a number using string conversion.
  abs() is used for negative numbers."
  [n]
  (check-int n)
  (count (str (abs (bigint n)))))

(defn benchmark
  [& _args]
  ;; Upstream benchmark only prints timeit timings; there is no portable
  ;; pure-Clojure behavior, so it is kept as a stub.
  (throw (ex-info "benchmark requires timeit; no port applicable"
                  {:type :UnsupportedOperation})))
