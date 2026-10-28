(ns clojure-algorithms.maths.average-mean
  "Arithmetic mean of a list of numbers.
Wiki: https://en.wikipedia.org/wiki/Mean")

(defn mean
  "Find the mean of a list of numbers."
  [nums]
  (when (empty? nums)
    (throw (ex-info "List is empty" {:type :ValueError})))
  (double (/ (reduce + nums) (count nums))))
