(ns clojure-algorithms.data-structures.binary-tree.binary-search-tree-recursive-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-search-tree-recursive
             :refer [bst-put bst-search bst-exists? bst-remove bst-max-label
                     bst-min-label inorder-labels preorder-labels bst-empty
                     bst-empty?]]))

(defn- make-test-tree []
  (reduce bst-put nil [8 3 6 1 10 14 13 4 7 5]))

(deftest test-put-search
  (is (true? (bst-empty? (bst-empty))))
  (let [t (bst-put nil 8)]
    (is (= 8 (:label t)))
    (is (false? (bst-empty? t))))
  (let [t (-> nil (bst-put 8) (bst-put 10) (bst-put 3) (bst-put 6) (bst-put 1))]
    (is (= 10 (:label (:right t))))
    (is (= 3 (:label (:left t))))
    (is (= 6 (:label (:right (:left t)))))
    (is (= 1 (:label (:left (:left t)))))
    (is (thrown? clojure.lang.ExceptionInfo (bst-put t 1))))
  (let [t (make-test-tree)]
    (is (= 6 (:label (bst-search t 6))))
    (is (= 13 (:label (bst-search t 13))))
    (is (thrown? clojure.lang.ExceptionInfo (bst-search t 2)))
    (is (thrown? clojure.lang.ExceptionInfo (bst-remove t 2)))))

(deftest test-remove
  (let [t (bst-remove (make-test-tree) 13)]
    (is (= [1 3 4 5 6 7 8 10 14] (vec (inorder-labels t)))))
  (let [t (-> (make-test-tree) (bst-remove 13) (bst-remove 7) (bst-remove 6))]
    (is (= 4 (:label (:right (:left t)))))
    (is (= 5 (:label (:right (:right (:left t))))))
    (is (= [1 3 4 5 8 10 14] (vec (inorder-labels t)))))
  (let [t (bst-remove (make-test-tree) 3)]
    (is (= 4 (:label (:left t))))
    (is (= 6 (:label (:right (:left t)))))
    (is (= 1 (:label (:left (:left t)))))
    (is (= 7 (:label (:right (:right (:left t))))))
    (is (= 5 (:label (:left (:right (:left t))))))))

(deftest test-queries
  (let [t (make-test-tree)]
    (is (true? (bst-exists? t 6)))
    (is (false? (bst-exists? t -1)))
    (is (= 14 (bst-max-label t)))
    (is (= 1 (bst-min-label t)))
    (is (= [1 3 4 5 6 7 8 10 13 14] (vec (inorder-labels t))))
    (is (= [8 3 1 6 4 5 7 10 14 13] (vec (preorder-labels t)))))
  (is (thrown? clojure.lang.ExceptionInfo (bst-max-label nil)))
  (is (thrown? clojure.lang.ExceptionInfo (bst-min-label nil))))
