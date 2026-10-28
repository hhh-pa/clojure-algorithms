(ns clojure-algorithms.machine-learning.frequent-pattern-growth-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.frequent-pattern-growth :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/create-tree))
  (is (some? #'sut/update-tree))
  (is (some? #'sut/update-header))
  (is (some? #'sut/ascend-tree))
  (is (some? #'sut/find-prefix-path))
  (is (some? #'sut/mine-tree))
  (is (some? #'sut/make-TreeNode))
  (is (some? #'sut/TreeNode-repr))
  (is (some? #'sut/TreeNode-inc))
  (is (some? #'sut/TreeNode-disp)))
