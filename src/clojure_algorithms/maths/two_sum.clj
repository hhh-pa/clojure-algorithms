(ns clojure-algorithms.maths.two-sum
  "Given an array of integers, return indices of the two numbers such that
they add up to a specific target.

Example: nums = [2, 7, 11, 15], target = 9 -> [0, 1],
because nums[0] + nums[1] = 2 + 7 = 9.")

(defn two-sum
  "Returns the indices [i j] of the two numbers adding up to target,
or an empty vector when there is no solution."
  [nums target]
  (loop [seen {}, idx 0, remaining nums]
    (if (empty? remaining)
      []
      (let [val (first remaining)
            compl (- target val)]
        (if (contains? seen compl)
          [(seen compl) idx]
          (recur (assoc seen val idx) (inc idx) (rest remaining)))))))
