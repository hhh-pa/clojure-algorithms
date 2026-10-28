(ns clojure-algorithms.data-structures.arrays.sparse-table-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.sparse-table
             :refer [build-sparse-table sparse-query]]))

(deftest test-build-sparse-table
  (is (= [[8 1 0 3 4 9 3] [1 0 0 3 4 3 0] [0 0 0 3 0 0 0]]
         (build-sparse-table [8 1 0 3 4 9 3])))
  (is (= [[3 1 9] [1 1 0]] (build-sparse-table [3 1 9])))
  (is (thrown? clojure.lang.ExceptionInfo (build-sparse-table []))))

(deftest test-sparse-query
  (is (= 0 (sparse-query (build-sparse-table [8 1 0 3 4 9 3]) 0 4)))
  (is (= 3 (sparse-query (build-sparse-table [8 1 0 3 4 9 3]) 4 6)))
  (is (= 9 (sparse-query (build-sparse-table [3 1 9]) 2 2)))
  (is (= 1 (sparse-query (build-sparse-table [3 1 9]) 0 1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (sparse-query (build-sparse-table [8 1 0 3 4 9 3]) 0 11)))
  (is (thrown? clojure.lang.ExceptionInfo (sparse-query [[]] 0 0))))
