(ns clojure-algorithms.maths.average-absolute-deviation
  "Average absolute deviation of a list of numbers.
  See https://en.wikipedia.org/wiki/Average_absolute_deviation")

(defn average-absolute-deviation
  "Returns the average absolute deviation of the numbers in `nums`:
  the mean of the absolute differences between each value and the mean."
  [nums]
  (when (empty? nums)
    (throw (ex-info "List is empty" {:type :ValueError})))
  (let [n (count nums)
        mean (/ (double (reduce + nums)) n)]
    (/ (reduce + (map #(Math/abs (- % mean)) nums)) n)))
