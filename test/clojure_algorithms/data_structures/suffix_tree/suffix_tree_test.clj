(ns clojure-algorithms.data-structures.suffix-tree.suffix-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.suffix-tree.suffix-tree
             :refer [make-suffix-tree suffix-search]]))

(deftest test-suffix-tree-search
  (let [tree (make-suffix-tree "banana")]
    (doseq [p ["ana" "ban" "na"]]
      (is (true? (suffix-search tree p))))
    (doseq [p ["xyz" "apple" "cat"]]
      (is (false? (suffix-search tree p))))
    (is (true? (suffix-search tree ""))))
  (let [tree (make-suffix-tree "monkey banana")]
    (doseq [p ["ana" "ban" "na" "mon"]]
      (is (true? (suffix-search tree p))))
    (is (false? (suffix-search tree "xyz"))))
  (let [tree (make-suffix-tree "")]
    (is (true? (suffix-search tree "")))
    (is (false? (suffix-search tree "a")))))
