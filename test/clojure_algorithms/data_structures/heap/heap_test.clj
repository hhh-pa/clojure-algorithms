(ns clojure-algorithms.data-structures.heap.heap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.heap
             :refer [make-heap heap-values parent-index left-child-idx
                     right-child-idx max-heapify build-max-heap extract-max
                     heap-insert heap-sort]]))

(deftest test-classic-heap
  (let [h (build-max-heap [103 9 1 7 11 15 25 201 209 107 5])]
    (is (= [209 201 25 103 107 15 1 9 7 11 5] (heap-values h)))
    (let [[m h] (extract-max h)]
      (is (= 209 m))
      (is (= [201 107 25 103 11 15 1 9 7 5] (heap-values h)))
      (let [h (heap-insert h 100)]
        (is (= [201 107 25 103 100 15 1 9 7 5 11] (heap-values h)))
        (is (= [1 5 7 9 11 15 25 100 103 107 201]
               (heap-values (heap-sort h))))))))

(deftest test-build-heapify
  (is (= [50 40 20 20 10]
         (heap-values (build-max-heap [20 40 50 20 10]))))
  (is (= [9 8 7 4 5 6 3 2 1 0]
         (heap-values (build-max-heap [1 2 3 4 5 6 7 8 9 0]))))
  (is (= [514 57 105 5 8 99 61]
         (heap-values (build-max-heap [514 5 61 57 8 99 105]))))
  (is (= [514 57 105 5 8 9.9 61.6]
         (heap-values (build-max-heap [514 5 61.6 57 8 9.9 105]))))
  (is (= [10] (heap-values (heap-insert (make-heap) 10))))
  (is (= [10 10] (heap-values (-> (make-heap)
                                  (heap-insert 10) (heap-insert 10)))))
  (is (= [10.1 10] (heap-values (-> (make-heap)
                                    (heap-insert 10) (heap-insert 10.1)))))
  (is (= [9 5 0.1 0] (heap-values (reduce heap-insert (make-heap)
                                          [0.1 0 9 5])))))

(deftest test-indexes
  (let [h (build-max-heap [103 9 1 7 11 15 25 201 209 107 5])]
    (is (nil? (parent-index h -1)))
    (is (nil? (parent-index h 0)))
    (is (= 0 (parent-index h 1)))
    (is (= 0 (parent-index h 2)))
    (is (= 1 (parent-index h 3)))
    (is (= 1 (parent-index h 4)))
    (is (= 2 (parent-index h 5)))
    (is (= 4.0 (parent-index h 10.5)))
    (is (= 104.0 (parent-index h 209.0)))
    (is (thrown? clojure.lang.ExceptionInfo (parent-index h "Test")))
    (is (= 1 (left-child-idx h 0)))
    (is (= 2 (right-child-idx h 0)))
    (is (nil? (left-child-idx h 10)))))

(deftest test-extract-errors
  (is (= 50 (first (extract-max (build-max-heap [20 40 50 20 10])))))
  (is (= 514 (first (extract-max (build-max-heap [514 5 61 57 8 99 105])))))
  (is (= 9 (first (extract-max (build-max-heap [1 2 3 4 5 6 7 8 9 0])))))
  (is (thrown? clojure.lang.ExceptionInfo (extract-max (make-heap)))))
