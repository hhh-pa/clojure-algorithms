(ns clojure-algorithms.linear-algebra.src.lib-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.lib
             :refer [angle axpy cofactor component determinant dot euclidean-length
                     madd mat-vec minor-matrix mscale msub random-matrix random-vector
                     square-zero-matrix unit-basis-vector vadd vscale vsub zero-vector]]))

(deftest test-vectors
  (is (= [0 0 0] (zero-vector 3)))
  (is (= [0 1 0] (unit-basis-vector 3 1)))
  (is (= [5 7 9] (vadd [1 2 3] [4 5 6])))
  (is (= [-3 -3 -3] (vsub [1 2 3] [4 5 6])))
  (is (= [2 4 6] (vscale [1 2 3] 2)))
  (is (= 32 (dot [1 2 3] [4 5 6])))
  (is (= [6 9 12] (axpy 2 [1 2 3] [4 5 6])))
  (is (= 5.385164807134504 (euclidean-length [2 3 4])))
  (is (= 1.0 (euclidean-length [1])))
  (is (= 1.4906464636572374 (angle [3 4 -1] [2 -1 1])))
  (is (= 85.40775111366095 (angle [3 4 -1] [2 -1 1] true)))
  (is (thrown? clojure.lang.ExceptionInfo (euclidean-length [])))
  (is (thrown? clojure.lang.ExceptionInfo (vadd [1] [1 2])))
  (let [v (random-vector 5 0 10)]
    (is (= 5 (count v)))
    (is (every? #(<= 0 % 10) v))))

(deftest test-matrices
  (is (= [[0 0] [0 0]] (square-zero-matrix 2)))
  (is (= [[6 8] [10 12]] (madd [[1 2] [3 4]] [[5 6] [7 8]])))
  (is (= [[-4 -4] [-4 -4]] (msub [[1 2] [3 4]] [[5 6] [7 8]])))
  (is (= [[2 4] [6 8]] (mscale [[1 2] [3 4]] 2)))
  (is (= [14 32] (mat-vec [[1 2 3] [4 5 6]] [1 2 3])))
  (is (= 2 (component [[1 2] [3 4]] 0 1)))
  (is (= -2 (determinant [[1 2] [3 4]])))
  (is (== 1 (determinant [[1 2 3] [0 1 4] [5 6 0]])))
  (is (= 5 (determinant [[5]])))
  (is (= -3.0 (cofactor [[1 2] [3 4]] 0 1)))
  (is (= 4 (minor-matrix [[1 2] [3 4]] 0 0)))
  (is (thrown? clojure.lang.ExceptionInfo (determinant [[1 2 3] [4 5 6]])))
  (let [m (random-matrix 3 2 0 5)]
    (is (= 2 (count m)))
    (is (= 3 (count (first m))))))
