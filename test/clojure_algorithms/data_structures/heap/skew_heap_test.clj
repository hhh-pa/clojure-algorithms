(ns clojure-algorithms.data-structures.heap.skew-heap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.skew-heap
             :refer [merge-nodes heap-insert heap-top heap-pop to-sorted-list
                     make-skew-heap]]))

(deftest test-skew-heap
  (is (= [1 1 2 3 5 7]
         (to-sorted-list (make-skew-heap [2 3 1 5 1 7]))))
  (is (thrown? clojure.lang.ExceptionInfo (heap-pop nil)))
  (is (= [-1 0 1] (to-sorted-list (reduce heap-insert nil [1 -1 0]))))
  (is (= -10.5 (:value (merge-nodes {:value 10 :left nil :right nil}
                                    {:value -10.5 :left nil :right nil}))))
  (is (= 10 (:value (merge-nodes {:value 10 :left nil :right nil}
                                 {:value 10.5 :left nil :right nil}))))
  (is (= 10 (:value (merge-nodes {:value 10 :left nil :right nil}
                                 {:value 10 :left nil :right nil}))))
  (is (= -100 (:value (merge-nodes {:value -100 :left nil :right nil}
                                   {:value -10.5 :left nil :right nil})))))
