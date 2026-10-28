(ns clojure-algorithms.maths.two-pointer
  "Given a sorted array of integers, return indices of the two numbers such
  that they add up to a specific target using the two pointers technique.

  You may assume that each input would have exactly one solution, and you
  may not use the same element twice.

  This is an alternative solution of the two-sum problem, which uses a
  map to solve the problem. Hence can not solve the issue if there is a
  constraint not use the same index twice. [1]

  Example:
  Given nums = [2, 7, 11, 15], target = 9,

  Because nums[0] + nums[1] = 2 + 7 = 9,
  return [0, 1].

  [1]: https://github.com/TheAlgorithms/Python/blob/master/other/two_sum.py")

(defn two-pointer
  "Returns the indices of the two numbers adding up to target,
  or an empty vector when there is no such pair."
  [nums target]
  (loop [i 0 j (dec (count nums))]
    (if (>= i j)
      []
      (let [s (+ (nth nums i) (nth nums j))]
        (cond (= s target) [i j]
              (< s target) (recur (inc i) j)
              :else (recur i (dec j)))))))
