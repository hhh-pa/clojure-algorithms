(ns clojure-algorithms.data-structures.binary-tree.binary-tree-mirror-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-tree-mirror
             :refer [binary-tree-mirror]]))

(deftest test-binary-tree-mirror
  (is (= {1 [3 2] 2 [5 4] 3 [7 6] 7 [9 8]}
         (binary-tree-mirror {1 [2 3] 2 [4 5] 3 [6 7] 7 [8 9]} 1)))
  (is (= {1 [3 2] 2 [5 4] 3 [7 6] 4 [11 10]}
         (binary-tree-mirror {1 [2 3] 2 [4 5] 3 [6 7] 4 [10 11]} 1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (binary-tree-mirror {1 [2 3] 2 [4 5] 3 [6 7] 4 [10 11]} 5)))
  (is (thrown? clojure.lang.ExceptionInfo (binary-tree-mirror {} 5))))
