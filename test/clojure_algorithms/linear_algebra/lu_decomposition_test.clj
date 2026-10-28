(ns clojure-algorithms.linear-algebra.lu-decomposition-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.lu-decomposition :refer [lower-upper-decomposition]]))

(defn- mat-approx [a b]
  (every? true? (map (fn [ra rb] (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) ra rb))) a b)))

(deftest test-lu-decomposition
  (let [[l u] (lower-upper-decomposition [[2 -2 1] [0 1 2] [5 3 1]])]
    (is (mat-approx [[1.0 0.0 0.0] [0.0 1.0 0.0] [2.5 8.0 1.0]] l))
    (is (mat-approx [[2.0 -2.0 1.0] [0.0 1.0 2.0] [0.0 0.0 -17.5]] u)))
  (let [[l u] (lower-upper-decomposition [[4 3] [6 3]])]
    (is (mat-approx [[1.0 0.0] [1.5 1.0]] l))
    (is (mat-approx [[4.0 3.0] [0.0 -1.5]] u)))
  (let [[l u] (lower-upper-decomposition [[1 0] [1 0]])]
    (is (mat-approx [[1.0 0.0] [1.0 1.0]] l))
    (is (mat-approx [[1.0 0.0] [0.0 0.0]] u)))
  (is (thrown? clojure.lang.ExceptionInfo
               (lower-upper-decomposition [[2 -2 1] [0 1 2]])))
  (is (thrown? clojure.lang.ExceptionInfo
               (lower-upper-decomposition [[0 1] [1 0]])))
  (is (thrown? clojure.lang.ExceptionInfo
               (lower-upper-decomposition [[0 1] [0 1]]))))
