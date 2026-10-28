(ns clojure-algorithms.linear-algebra.src.rank-of-matrix-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.rank-of-matrix :refer [rank-of-matrix]]))

(deftest test-rank-of-matrix
  (is (= 2 (rank-of-matrix [[1 2 3] [4 5 6] [7 8 9]])))
  (is (= 2 (rank-of-matrix [[1 0 0] [0 1 0] [0 0 0]])))
  (is (= 2 (rank-of-matrix [[1 2 3 4] [5 6 7 8] [9 10 11 12]])))
  (is (= 4 (rank-of-matrix [[2 3 -1 -1] [1 -1 -2 4] [3 1 3 -2] [6 3 0 -7]])))
  (is (= 3 (rank-of-matrix [[2 1 -3 -6] [3 -3 1 2] [1 1 1 2]])))
  (is (= 3 (rank-of-matrix [[2 -1 0] [1 3 4] [4 1 -3]])))
  (is (= 1 (rank-of-matrix [[3 2 1] [-6 -4 -2]])))
  (is (= 0 (rank-of-matrix [[0 0] [0 0]])))
  (is (= 1 (rank-of-matrix [[1]]))))
