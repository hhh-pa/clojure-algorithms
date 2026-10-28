(ns clojure-algorithms.maths.average-mean
  "Port of maths/average_mean.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare mean)

(defn mean "Find mean of a list of numbers.\nWiki: https://en.wikipedia.org/wiki/Mean\n\n>>> mean([3, 6, 9, 12, 15, 18, 21])\n12.0\n>>> mean([5, 10, 15, 20, 25, 30, 35])\n20.0\n>>> mean([1, 2, 3, 4, 5, 6, 7, 8])\n4.5\n>>> mean([])\nTraceback (most recent call last):\n    ...\nValueError: List is empty"
  [nums] (try (when (py-truthy (not (py-truthy nums))) (throw (ex-info (py-str "List is empty") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (reduce +' 0 nums) (count nums))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
