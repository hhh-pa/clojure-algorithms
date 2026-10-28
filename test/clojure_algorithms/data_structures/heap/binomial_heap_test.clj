(ns clojure-algorithms.data-structures.heap.binomial-heap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.binomial-heap
             :refer [make-binomial-heap heap-size heap-empty? heap-insert
                     heap-merge heap-peek heap-delete-min heap-drain]]))

(deftest test-binomial-heap
  (let [perm [7 23 1 19 11 29 5 13 17 3 25 9 27 15 21 0 6 12 18 24
              2 8 14 20 26 4 10 16 22 28]
        h (reduce heap-insert (make-binomial-heap) perm)]
    (is (= 30 (heap-size h)))
    (is (= 0 (heap-peek h)))
    (let [[mins h] (loop [h h mins []]
                     (if (= 20 (count mins))
                       [mins h]
                       (let [[m h] (heap-delete-min h)]
                         (recur h (conj mins m)))))]
      (is (= (range 20) mins))
      (let [second (reduce heap-insert (make-binomial-heap) [17 20 31 34])]
        (is (= 17 (heap-peek second)))
        (is (= [17 20 31 34] (heap-drain second)))
        (let [merged (heap-merge second h)]
          (is (= 17 (heap-peek merged)))
          (is (= [17 20 20 21 22 23 24 25 26 27 28 29 31 34]
                 (heap-drain merged)))))))
  (let [h (make-binomial-heap)]
    (is (true? (heap-empty? h)))
    (let [[m h2] (heap-delete-min (heap-insert h 1))]
      (is (= 1 m))
      (is (true? (heap-empty? h2))))))
