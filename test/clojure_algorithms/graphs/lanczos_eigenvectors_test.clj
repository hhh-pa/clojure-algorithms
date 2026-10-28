(ns clojure-algorithms.graphs.lanczos-eigenvectors-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.lanczos-eigenvectors
             :refer [validate-adjacency-list multiply-matrix-vector
                     lanczos-iteration find-lanczos-eigenvectors]]))

(deftest test-lanczos
  (is (nil? (validate-adjacency-list [[1 2] [0] [0 1]])))
  (is (thrown? IllegalArgumentException
               (validate-adjacency-list [[1] [2] [-1]])))
  (is (= [2 2 2] (multiply-matrix-vector [[1 2] [0 2] [0 1]] [1 1 1])))
  (let [[t q] (lanczos-iteration [[1 2] [0 2] [0 1]] 2
                                 :initial [1.0 0.0 0.0])]
    (is (= [2 2] [(count t) (count (first t))]))
    (is (= [2 3] [(count q) (count (first q))])))
  (let [[vals vecs] (find-lanczos-eigenvectors [[1 2] [0 2] [0 1]] 2
                                               :initial [1.0 0.0 0.0])]
    (is (< (Math/abs (- (first vals) 2.0)) 1e-6))
    (is (< (Math/abs (- (second vals) -1.0)) 1e-6))
    (is (= 2 (count vecs)))
    (is (= 3 (count (first vecs))))))
