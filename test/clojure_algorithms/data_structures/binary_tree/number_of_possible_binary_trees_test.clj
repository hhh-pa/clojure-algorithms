(ns clojure-algorithms.data-structures.binary-tree.number-of-possible-binary-trees-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.number-of-possible-binary-trees
             :refer [binomial-coefficient catalan-number factorial
                     binary-tree-count]]))

(deftest test-counts
  (is (= 6 (binomial-coefficient 4 2)))
  (is (= 42 (catalan-number 5)))
  (is (= 132 (catalan-number 6)))
  (is (= [1 1 2 6 24 120 720 5040 40320 362880]
         (mapv factorial (range 10))))
  (is (thrown? clojure.lang.ExceptionInfo (factorial -5)))
  (is (= 5040 (binary-tree-count 5)))
  (is (= 95040 (binary-tree-count 6))))
