(ns clojure-algorithms.matrix.median-matrix
  "https://en.wikipedia.org/wiki/Median"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare median)

(defn median "Calculate the median of a sorted matrix.\n\nArgs:\n    matrix: A 2D matrix of integers.\n\nReturns:\n    The median value of the matrix.\n\nExamples:\n    >>> matrix = [[1, 3, 5], [2, 6, 9], [3, 6, 9]]\n    >>> median(matrix)\n    5\n\n    >>> matrix = [[1, 2, 3], [4, 5, 6]]\n    >>> median(matrix)\n    3"
  [matrix] (let [linear (atom nil) mid (atom nil)] (try (reset! linear (py-sorted (doall (for [row (py-iter matrix) x (py-iter row)] x)))) (reset! mid (py-floordiv (py-sub (count @linear) 1) 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @linear @mid)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
