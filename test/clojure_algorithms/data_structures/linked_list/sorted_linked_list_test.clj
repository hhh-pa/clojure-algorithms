(ns clojure-algorithms.data-structures.linked-list.sorted-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.sorted-linked-list
             :refer [make-node node->str make-list list-empty? sorted-insert
                     sorted-delete list-contains? min-value max-value
                     remove-duplicates merge-other]]))

(deftest test-sorted-list
  (is (= "Node(data=1, next=Node(data=2, next=Node(data=3, next=None)))"
         (node->str (make-node 1 (make-node 2 (make-node 3))))))
  (is (true? (list-empty? (make-list))))
  (is (= [] (make-list)))
  (let [lst (-> (make-list) (sorted-insert 3) (sorted-insert 1)
                (sorted-insert 2))]
    (is (= [1 2 3] lst))
    (is (= 3 (count lst))))
  (let [lst (sorted-insert (make-list) 3)]
    (is (true? (list-contains? lst 3)))
    (is (false? (list-contains? lst 1))))
  (let [lst (-> (make-list) (sorted-insert 32) (sorted-insert 57)
                (sorted-insert 45))]
    (is (= [32 45 57] lst))
    (let [[ok lst] (sorted-delete lst 45)]
      (is (true? ok))
      (is (= [32 57] lst)))
    (is (= [false [32 45 57]] [(first (sorted-delete lst 99))
                               (second (sorted-delete lst 99))])))
  (is (true? (list-empty? (make-list))))
  (let [lst (-> (make-list) (sorted-insert 32) (sorted-insert 57)
                (sorted-insert 45))]
    (is (false? (list-empty? lst)))
    (is (= 32 (min-value lst)))
    (is (= 57 (max-value lst))))
  (is (nil? (min-value (make-list))))
  (is (nil? (max-value (make-list))))
  (let [lst (-> (make-list) (sorted-insert 32) (sorted-insert 57)
                (sorted-insert 45) (sorted-insert 45))]
    (is (= [32 45 45 57] lst))
    (is (= [32 45 57] (remove-duplicates lst))))
  (let [lst (merge-other (make-list) [3 1])]
    (is (= [1 3] lst))))
