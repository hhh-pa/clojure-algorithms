(ns clojure-algorithms.data-structures.heap.heap-generic-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.heap.heap-generic
             :refer [make-heap insert-item update-item delete-item get-top
                     extract-top]]))

(deftest test-max-heap
  (let [h (-> (make-heap) (insert-item 5 34) (insert-item 6 31)
              (insert-item 7 37))]
    (is (= [7 37] (get-top h)))
    (let [[t h] (extract-top h)]
      (is (= [7 37] t))
      (let [[t h] (extract-top h)]
        (is (= [5 34] t))
        (let [[t _] (extract-top h)]
          (is (= [6 31] t)))))))

(deftest test-min-heap
  (let [h (-> (make-heap (fn [x] (- x)))
              (insert-item 5 34) (insert-item 6 31) (insert-item 7 37))]
    (is (= [6 -31] (get-top h)))
    (let [[t h] (extract-top h)]
      (is (= [6 -31] t))
      (let [[t h] (extract-top h)]
        (is (= [5 -34] t))
        (let [[t h] (extract-top h)]
          (is (= [7 -37] t))
          (let [h (-> h (insert-item 8 45) (insert-item 9 40)
                      (insert-item 10 50))]
            (is (= [9 -40] (get-top h)))
            (let [h (update-item h 10 30)]
              (is (= [10 -30] (get-top h)))
              (let [h (delete-item h 10)]
                (is (= [9 -40] (get-top h)))))))))))
