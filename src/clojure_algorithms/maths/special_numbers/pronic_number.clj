(ns clojure-algorithms.maths.special-numbers.pronic-number
  "== Pronic Number ==
A number n is said to be a Proic number if
there exists an integer m such that n = m * (m + 1)

Examples of Proic Numbers: 0, 2, 6, 12, 20, 30, 42, 56, 72, 90, 110 ...
https://en.wikipedia.org/wiki/Pronic_number")

(defn- isqrt
  "Integer square root of n: the greatest integer r with r*r <= n.
Assumes n is a non-negative integer."
  [n]
  (loop [x (long (Math/sqrt (double n)))]
    (cond
      (> (*' x x) n) (recur (dec x))
      (<= (*' (inc x) (inc x)) n) (recur (inc x))
      :else x)))

(defn is-pronic
  "Returns true if `number` is pronic, i.e. some integer m exists with
n = m * (m + 1). Returns false for negative and odd integers. Throws an
ex-info with :type :TypeError when `number` is not an integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (if (or (neg? number) (odd? number))
    false
    (let [root (isqrt number)]
      (= number (*' root (inc root))))))
