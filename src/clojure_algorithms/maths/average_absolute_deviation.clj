(ns clojure-algorithms.maths.average-absolute-deviation
  "Port of maths/average_absolute_deviation.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare average-absolute-deviation)

(defn average-absolute-deviation "Return the average absolute deviation of a list of numbers.\nWiki: https://en.wikipedia.org/wiki/Average_absolute_deviation\n\n>>> average_absolute_deviation([0])\n0.0\n>>> average_absolute_deviation([4, 1, 3, 2])\n1.0\n>>> average_absolute_deviation([2, 70, 6, 50, 20, 8, 4, 0])\n20.0\n>>> average_absolute_deviation([-20, 0, 30, 15])\n16.25\n>>> average_absolute_deviation([])\nTraceback (most recent call last):\n    ...\nValueError: List is empty"
  [nums] (let [average (atom nil)] (try (when (py-truthy (not (py-truthy nums))) (throw (ex-info (py-str "List is empty") {:type :ValueError}))) (reset! average (py-div (reduce +' 0 nums) (count nums))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (reduce +' 0 (doall (for [x (py-iter nums)] (py-abs (py-sub x @average))))) (count nums))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
