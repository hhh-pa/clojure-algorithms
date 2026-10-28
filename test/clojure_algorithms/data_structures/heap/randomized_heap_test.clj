(ns clojure-algorithms.data-structures.heap.randomized-heap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.randomized-heap
             :refer [merge-nodes heap-insert heap-top heap-pop to-sorted-list
                     make-randomized-heap]]))

(deftest test-randomized-heap
  (is (= [1 1 2 3 5 7]
         (to-sorted-list (make-randomized-heap [2 3 1 5 1 7]))))
  (is (thrown? clojure.lang.ExceptionInfo (heap-pop nil)))
  (is (= [-1 0 1]
         (to-sorted-list (reduce heap-insert nil [1 -1 0]))))
  (is (= [1 3 3 7] (to-sorted-list (make-randomized-heap [3 1 3 7]))))
  (is (= 10 (:value (merge-nodes {:value 10 :left nil :right nil}
                                 {:value 20 :left nil :right nil}))))
  (is (= 10 (:value (merge-nodes {:value 20 :left nil :right nil}
                                 {:value 10 :left nil :right nil}))))
  (is (= 0 (:value (merge-nodes {:value 5 :left nil :right nil}
                                {:value 0 :left nil :right nil}))))
  (let [[m root] (heap-pop (make-randomized-heap [3 1 2]))]
    (is (= 1 m))
    (is (= 2 (heap-top root)))))
