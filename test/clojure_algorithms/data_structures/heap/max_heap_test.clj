(ns clojure-algorithms.data-structures.heap.max-heap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.max-heap
             :refer [make-binary-heap heap-insert heap-pop heap-list heap-len]]))

(deftest test-binary-heap
  (let [h (reduce heap-insert (make-binary-heap) [6 10 15 12])
        [m1 h] (heap-pop h)
        [m2 h] (heap-pop h)]
    (is (= 15 m1))
    (is (= 12 m2))
    (is (= [10 6] (heap-list h)))
    (is (= 2 (heap-len h)))))
