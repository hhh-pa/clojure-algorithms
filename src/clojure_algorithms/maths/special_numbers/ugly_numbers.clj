(ns clojure-algorithms.maths.special-numbers.ugly-numbers
  "Ugly numbers are numbers whose only prime factors are 2, 3 or 5. The sequence
1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, ... shows the first 11 ugly numbers. By convention,
1 is included.
Given an integer n, we have to find the nth ugly number.

For more details, refer this article
https://www.geeksforgeeks.org/ugly-numbers/")

(defn ugly-numbers
  "Returns the nth ugly number (1-indexed; by convention the 0th and negative
positions also yield 1). Throws an ex-info with :type :TypeError when n is
not an integer."
  [n]
  (when-not (integer? n)
    (throw (ex-info (str "'" (cond
                               (float? n) "float"
                               (string? n) "str"
                               :else "object")
                         "' object cannot be interpreted as an integer")
                    {:type :TypeError})))
  (loop [ugly [1], i2 0, i3 0, i5 0]
    (if (>= (count ugly) (max n 1))
      (peek ugly)
      (let [next-2 (* (nth ugly i2) 2)
            next-3 (* (nth ugly i3) 3)
            next-5 (* (nth ugly i5) 5)
            next-num (min next-2 next-3 next-5)]
        (recur (conj ugly next-num)
               (if (= next-num next-2) (inc i2) i2)
               (if (= next-num next-3) (inc i3) i3)
               (if (= next-num next-5) (inc i5) i5))))))
