(ns clojure-algorithms.data-structures.binary-tree.binary-search-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-search-tree
             :refer [bst-insert bst-search bst-contains? bst-max bst-min bst-remove
                     inorder-values preorder-values postorder-values traversal-tree
                     find-kth-smallest bst-empty?]]))

(def ^:private testlist [8 3 6 1 10 14 13 4 7])

(defn- make-test-tree [] (apply bst-insert nil testlist))

(deftest test-traversals
  (let [t (make-test-tree)]
    (is (= [8 3 1 6 4 7 10 14 13] (vec (traversal-tree t))))
    (is (= [1 3 4 6 7 8 10 13 14] (vec (traversal-tree t :inorder))))
    (is (= [1 3 4 6 7 8 10 13 14] (vec (inorder-values t))))
    (is (= [1 4 7 6 3 13 14 10 8] (vec (traversal-tree t :postorder))))
    (is (= 4 (find-kth-smallest t 3)))))

(deftest test-search-min-max
  (let [t (make-test-tree)]
    (is (some? (bst-search t 6)))
    (is (true? (bst-contains? t 6)))
    (is (nil? (bst-search t -1)))
    (is (false? (bst-contains? t -1)))
    (is (= 14 (:value (bst-max t))))
    (is (= 14 (apply max (inorder-values t))))
    (is (= 1 (:value (bst-min t))))
    (is (= 1 (apply min (inorder-values t))))
    (is (thrown? clojure.lang.ExceptionInfo (bst-search nil 6)))))

(deftest test-remove
  (let [t (make-test-tree)]
    (is (thrown? clojure.lang.ExceptionInfo (bst-remove t 20)))
    (is (= [1 3 4 6 7 8 10 13] (vec (inorder-values (bst-remove t 14)))))
    (is (= [1 3 4 6 7 8 10 13 14]
           (vec (inorder-values (bst-remove (bst-insert t 7) 7)))))
    (let [cleared (reduce bst-remove t testlist)]
      (is (true? (bst-empty? cleared)))))
  (is (false? (bst-empty? (make-test-tree))))
  (is (true? (bst-empty? nil))))
