(ns clojure-algorithms.maths.special-numbers.deficient-numbers
  "A number n is said to be a Deficient number if
the sum of its proper divisors is less than the number itself.

Examples of Deficient Numbers: 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, ...

https://en.wikipedia.org/wiki/Deficient_number")

(defn- isqrt
  "Integer square root of n: the greatest integer r with r*r <= n.
Assumes n is a non-negative integer."
  [n]
  (loop [x (long (Math/sqrt (double n)))]
    (cond
      (> (*' x x) n) (recur (dec x))
      (<= (*' (inc x) (inc x)) n) (recur (inc x))
      :else x)))

(defn- proper-divisor-sum
  "Sum of the proper divisors of n. Assumes n is an integer >= 2."
  [n]
  (let [limit (isqrt n)]
    (reduce +'
            1
            (mapcat (fn [i]
                      (let [j (quot n i)]
                        (if (= i j) [i] [i j])))
                    (filter #(zero? (rem n %)) (range 2 (inc limit)))))))

(defn is-deficient-number
  "Returns true if `number` is deficient, i.e. the sum of its proper divisors
is less than the number itself. Returns false for integers below 1.
Throws an ex-info with :type :TypeError when `number` is not an integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (cond
    (< number 1) false
    (= number 1) true
    :else (> number (proper-divisor-sum number))))
