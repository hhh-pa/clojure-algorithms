(ns clojure-algorithms.data-structures.linked-list.middle-element-of-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.middle-element-of-linked-list
             :refer [make-list push middle-element]]))

(deftest test-middle-element
  (is (nil? (middle-element (make-list))))
  (let [[_ lst] (reduce (fn [[_ l] v] (push l v)) [nil (make-list)]
                        [5 6 8 8 10 12 17 7 3 20 -20])]
    (is (= [-20 20 3 7 17 12 10 8 8 6 5] lst))
    (is (= 12 (middle-element lst))))
  (let [[_ lst] (push (make-list) 1)]
    (is (= 1 (middle-element lst))))
  (let [[_ lst] (reduce (fn [[_ l] v] (push l v)) [nil (make-list)] [1 2])]
    (is (= 1 (middle-element lst)))))
