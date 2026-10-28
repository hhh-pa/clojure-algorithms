(ns clojure-algorithms.linear-algebra.jacobi-iteration-method-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.jacobi-iteration-method
             :refer [jacobi-iteration-method strictly-diagonally-dominant?]]))

(defn- approx-vec [a b]
  (every? true? (map #(< (Math/abs (- %1 %2)) 1e-12) a b)))

(deftest test-jacobi
  (is (approx-vec [0.909375 -1.14375 -0.7484375]
                  (jacobi-iteration-method [[4 1 1] [1 5 2] [1 2 4]] [2 -6 -4]
                                           [0.5 -0.5 -0.5] 3)))
  (is (thrown? clojure.lang.ExceptionInfo
               (jacobi-iteration-method [[4 1 1] [1 5 2]] [2 -6 -4] [0.5 -0.5 -0.5] 3)))
  (is (thrown? clojure.lang.ExceptionInfo
               (jacobi-iteration-method [[4 1 1] [1 5 2] [1 2 4]] [2 -6] [0.5 -0.5 -0.5] 3)))
  (is (thrown? clojure.lang.ExceptionInfo
               (jacobi-iteration-method [[4 1 1] [1 5 2] [1 2 4]] [2 -6 -4] [0.5 -0.5] 3)))
  (is (thrown? clojure.lang.ExceptionInfo
               (jacobi-iteration-method [[4 1 1] [1 5 2] [1 2 4]] [2 -6 -4] [0.5 -0.5 -0.5] 0))))

(deftest test-dominance
  (is (true? (strictly-diagonally-dominant? [[4 1 1 2] [1 5 2 -6] [1 2 4 -4]])))
  (is (thrown? clojure.lang.ExceptionInfo
               (strictly-diagonally-dominant? [[4 1 1 2] [1 5 2 -6] [1 2 3 -4]]))))
