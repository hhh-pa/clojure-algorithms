(ns clojure-algorithms.maths.perfect-square
  "Port of maths/perfect_square.py.")

(defn- as-number
  "Coerces booleans like Python ints; returns nil for non-numbers."
  [n]
  (cond (number? n) n
        (instance? Boolean n) (if n 1 0)))

(defn- check-searchable
  "Returns n as a number, throwing a TypeError like upstream for
  non-numeric input (Python booleans count as ints)."
  [n]
  (let [v (as-number n)]
    (when (nil? v)
      (throw (ex-info (str "'<=' not supported between instances of 'int' and '"
                           (cond (nil? n) "NoneType"
                                 (string? n) "str"
                                 (sequential? n) "list"
                                 :else "non-numeric type")
                           "'")
                      {:type :TypeError})))
    v))

(defn perfect-square
  "Checks if a number is a perfect square number or not.
  Returns true if the number is a square number, otherwise false."
  [num]
  (let [num (as-number num)]
    (when (nil? num)
      (throw (ex-info "must be real number, not str" {:type :TypeError})))
    (let [root (Math/sqrt num)]
      (== (* root root) num))))

(defn perfect-square-binary-search
  "Checks if a number is a perfect square using binary search.
  Time complexity: O(Log(n)), space complexity: O(1)."
  [n]
  (let [n (check-searchable n)]
    (if (Double/isNaN (double n))
      false
      (loop [left 0 right n]
        (if (> left right)
          false
          ;; Python's (left + right) // 2 is floored division.
          (let [mid (if (integer? right)
                      (quot (+ left right) 2)
                      (Math/floor (/ (+ left right) 2)))
                sq (* mid mid)]
            (cond (== sq n) true
                  (> sq n) (recur left (dec mid))
                  :else (recur (inc mid) right))))))))
