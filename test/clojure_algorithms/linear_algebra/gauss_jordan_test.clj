(ns clojure-algorithms.linear-algebra.gauss-jordan-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.gauss-jordan :refer [gauss-jordan]]))

(defn- mat-approx [a b]
  (every? true? (map (fn [ra rb] (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) ra rb))) a b)))

(deftest test-gauss-jordan
  (let [[ra rb] (gauss-jordan [[1 2 -1] [2 4 -2] [3 6 -3]] [1 2 3])]
    (is (mat-approx [[1.0 2.0 -1.0] [0.0 0.0 0.0] [0.0 0.0 0.0]] ra))
    (is (mat-approx [[1.0] [0.0] [0.0]] (mapv vector rb))))
  (let [[ra rb] (gauss-jordan [[1 0] [0 1]] [5 7])]
    (is (mat-approx [[1.0 0.0] [0.0 1.0]] ra))
    (is (mat-approx [[5.0] [7.0]] (mapv vector rb))))
  (is (thrown? clojure.lang.ExceptionInfo (gauss-jordan [[1 2] [3 4]] [1 2 3]))))
