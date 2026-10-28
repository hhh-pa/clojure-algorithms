(ns clojure-algorithms.linear-algebra.src.schur-complement-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.schur-complement :refer [schur-complement]]))

(defn- mat-approx [a b]
  (every? true? (map (fn [ra rb] (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) ra rb))) a b)))

(deftest test-schur-complement
  (is (mat-approx [[5.0 -5.0] [0.0 6.0]]
                  (schur-complement [[1 2] [2 1]] [[0 3] [3 0]] [[2 1] [6 3]])))
  (is (thrown? clojure.lang.ExceptionInfo
               (schur-complement [[1 2 1] [2 1 2] [3 2 4]]
                                 [[0 3] [3 0] [2 3]]
                                 [[2 1 3] [6 3 5]])))
  (is (thrown? clojure.lang.ExceptionInfo
               (schur-complement [[1 1] [1 1]] [[1 0] [0 1]] [[1 0] [0 1]]))))
