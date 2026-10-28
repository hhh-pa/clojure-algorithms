(ns clojure-algorithms.data-structures.linked-list.merge-sort-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.merge-sort-linked-list
             :refer [make-node node->str chain-from-vec iter-linked-list
                     get-middle merge-chains merge-sort-linked-list]]))

(deftest test-merge-sort-list
  (let [head (make-node 3 (make-node 1 (make-node 2)))]
    (is (= "Node(data=3, next=Node(data=1, next=Node(data=2, next=None)))"
           (node->str head)))
    (is (= [3 1 2] (vec (iter-linked-list head)))))
  (let [head (chain-from-vec [1 2 3])]
    (is (= 2 (:data (get-middle head)))))
  (is (nil? (get-middle (chain-from-vec [1]))))
  (is (nil? (get-middle nil)))
  (let [left (chain-from-vec [1 3]) right (chain-from-vec [2 4])]
    (is (= [1 2 3 4] (vec (iter-linked-list (merge-chains left right))))))
  (is (= [1 1 2 3 4 5]
         (vec (iter-linked-list
               (merge-sort-linked-list (chain-from-vec [3 1 4 1 5 2]))))))
  (is (= [] (vec (iter-linked-list (merge-sort-linked-list nil)))))
  (is (= [7] (vec (iter-linked-list
                   (merge-sort-linked-list (chain-from-vec [7])))))))
