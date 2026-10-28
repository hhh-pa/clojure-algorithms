(ns clojure-algorithms.dynamic-programming.optimal-binary-search-tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.optimal-binary-search-tree :as bst]
            [clojure.string :as str]))

(deftest describe-bst-test
  (testing "tree description"
    (let [root [[0 1 1 1 1 1] [0 1 1 1 1 3] [0 0 2 3 3 3]
                [0 0 0 3 3 3] [0 0 0 0 4 5] [0 0 0 0 0 5]]]
      (is (= ["8 is the root of the binary search tree."
              "3 is the left child of key 8."
              "10 is the right child of key 8."
              "9 is the left child of key 10."
              "21 is the right child of key 10."
              "17 is the left child of key 21."]
             (bst/describe-bst root [3 8 9 10 17 21] 0 5))))))

(deftest find-optimal-bst-test
  (testing "optimal BST report"
    (let [nodes [{:key 12 :freq 8} {:key 10 :freq 34} {:key 20 :freq 50}
                 {:key 42 :freq 3} {:key 25 :freq 40} {:key 37 :freq 30}]
          report (bst/find-optimal-bst nodes)]
      (is (str/includes? report "The cost of optimal BST for given tree nodes is 324."))
      (is (str/includes? report "20 is the root of the binary search tree."))
      (is (str/includes? report "10 is the left child of key 20."))
      (is (str/includes? report "12 is the right child of key 10."))
      (is (str/includes? report "25 is the right child of key 20."))
      (is (str/includes? report "37 is the right child of key 25."))
      (is (str/includes? report "42 is the right child of key 37.")))))
