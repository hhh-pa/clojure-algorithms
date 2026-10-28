(ns clojure-algorithms.data-structures.linked-list.partition-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.partition-linked-list
             :refer [make-list ll-str add-item partition-linked-list]]))

(deftest test-partition
  (let [lst (-> (make-list) (add-item 23) (add-item 14) (add-item 9))]
    (is (= "9 --> 14 --> 23" (ll-str lst)))
    (is (= 3 (count lst))))
  (let [lst (reduce add-item (make-list) [5 1 4 2 3])]
    (is (= "2 --> 1 --> 3 --> 4 --> 5"
           (ll-str (partition-linked-list lst 3)))))
  (let [lst (reduce add-item (make-list) [1 2 3])]
    (is (= "2 --> 1 --> 3" (ll-str (partition-linked-list lst 3)))))
  (let [lst (reduce add-item (make-list) [5 5 5])]
    (is (= "5 --> 5 --> 5" (ll-str (partition-linked-list lst 5)))))
  (let [lst (-> (make-list) (add-item 1 0) (add-item 2 1))]
    (is (= "1 --> 2" (ll-str lst))))
  (is (= [] (partition-linked-list (make-list) 3))))
