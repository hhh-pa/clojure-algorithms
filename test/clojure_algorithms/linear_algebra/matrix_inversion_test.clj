(ns clojure-algorithms.linear-algebra.matrix-inversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.matrix-inversion :refer [invert-matrix]]))

(defn- rounded [m] (mapv (fn [row] (mapv #(Math/round (* % 1e6)) row)) m))

(deftest test-invert-matrix
  (is (= (rounded [[0.6 -0.7] [-0.2 0.4]])
         (rounded (invert-matrix [[4.0 7.0] [2.0 6.0]]))))
  (is (= (rounded [[1.0 0.0] [0.0 0.5]])
         (rounded (invert-matrix [[1.0 0.0] [0.0 2.0]]))))
  (is (thrown? clojure.lang.ExceptionInfo (invert-matrix [[1.0 2.0] [0.0 0.0]]))))
