(ns clojure-algorithms.backtracking.power-sum
  "Count the ways to write X as a sum of Nth powers of unique natural
  numbers (e.g. 13 = 2^2 + 3^2).
  Source: https://www.hackerrank.com/challenges/the-power-sum/problem")

(defn- int-pow
  [base exp]
  (reduce * 1 (repeat exp base)))

(defn backtrack
  "Searches completions from `current-number` upward. Returns
  [current-sum solutions-count]."
  [needed-sum power current-number current-sum solutions-count]
  (if (= current-sum needed-sum)
    [current-sum (inc solutions-count)]
    (let [i-to-n (int-pow current-number power)
          [current-sum solutions-count]
          (if (<= (+ current-sum i-to-n) needed-sum)
            (let [[s c] (backtrack needed-sum power (inc current-number)
                                   (+ current-sum i-to-n) solutions-count)]
              [(- s i-to-n) c])
            [current-sum solutions-count])
          [current-sum solutions-count]
          (if (< i-to-n needed-sum)
            (backtrack needed-sum power (inc current-number)
                       current-sum solutions-count)
            [current-sum solutions-count])]
      [current-sum solutions-count])))

(defn solve
  "Returns the number of ways to express `needed-sum` as a sum of unique
  `power`-th powers. 1 <= needed-sum <= 1000, 2 <= power <= 10."
  [needed-sum power]
  (when (not (and (<= 1 needed-sum 1000) (<= 2 power 10)))
    (throw (ex-info (str "Invalid input\n"
                         "needed_sum must be between 1 and 1000, "
                         "power between 2 and 10.") {})))
  (second (backtrack needed-sum power 1 0 0)))
