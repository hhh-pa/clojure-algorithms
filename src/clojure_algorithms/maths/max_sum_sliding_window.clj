(ns clojure-algorithms.maths.max-sum-sliding-window
  "Given an array of integer elements and an integer 'k', we are required to find the
maximum sum of 'k' consecutive elements in the array.

Instead of using a nested for loop, in a Brute force approach we will use a technique
called 'Window sliding technique' where the nested loops can be converted to a single
loop to reduce time complexity.")

(defn max-sum-in-array
  "Return the maximum sum of k consecutive elements of array using the
  sliding window technique. Throws :ValueError when k is negative or
  larger than the array."
  [array k]
  (when (or (< (count array) k) (< k 0))
    (throw (ex-info "Invalid Input" {:type :ValueError})))
  (let [initial (reduce + 0 (take k array))]
    (loop [i 0
           current-sum initial
           max-sum initial]
      (if (>= i (- (count array) k))
        max-sum
        (let [current-sum' (+ (- current-sum (nth array i))
                              (nth array (+ i k)))]
          (recur (inc i) current-sum' (max max-sum current-sum')))))))
