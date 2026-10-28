(ns clojure-algorithms.data-structures.binary-tree.diff-views-of-binary-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.diff-views-of-binary-tree
             :refer [make-tree right-side-view left-side-view top-side-view
                     bottom-side-view]]))

(deftest test-views
  (is (= 3 (:val (make-tree))))
  (is (= [3 20 7] (right-side-view (make-tree))))
  (is (= [] (right-side-view nil)))
  (is (= [3 9 15] (left-side-view (make-tree))))
  (is (= [] (left-side-view nil)))
  (is (= [9 3 20 7] (top-side-view (make-tree))))
  (is (= [] (top-side-view nil)))
  (is (= [9 15 20 7] (bottom-side-view (make-tree))))
  (is (= [] (bottom-side-view nil))))
