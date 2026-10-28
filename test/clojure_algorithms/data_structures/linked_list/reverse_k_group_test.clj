(ns clojure-algorithms.data-structures.linked-list.reverse-k-group-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.reverse-k-group
             :refer [make-list ll-str append-item reverse-k-nodes]]))

(deftest test-reverse-k-group
  (is (= [] (make-list [])))
  (is (= [0 1 2 3 4] (make-list (range 5))))
  (is (= 0 (count (make-list (range 0)))))
  (is (= 8 (count (make-list "abcdefgh"))))
  (is (= "" (ll-str (make-list []))))
  (is (= "0 -> 1 -> 2 -> 3 -> 4" (ll-str (make-list (range 5)))))
  (let [lst (-> (make-list [1 2]) (append-item 3))]
    (is (= [1 2 3] lst)))
  (is (= [2 1 4 3 5] (reverse-k-nodes (make-list [1 2 3 4 5]) 2)))
  (is (= "2 -> 1 -> 4 -> 3 -> 5"
         (ll-str (reverse-k-nodes (make-list [1 2 3 4 5]) 2))))
  (is (= [3 2 1 4 5] (reverse-k-nodes (make-list [1 2 3 4 5]) 3)))
  (is (= [1 2 3] (reverse-k-nodes (make-list [1 2 3]) 1)))
  (is (= [] (reverse-k-nodes (make-list []) 2)))
  (is (= [7] (reverse-k-nodes (make-list [7]) 3))))
