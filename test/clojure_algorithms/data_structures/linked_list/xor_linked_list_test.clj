(ns clojure-algorithms.data-structures.linked-list.xor-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.xor-linked-list
             :refer [make-list insert-value to-list]]))

(deftest test-xor-list
  (let [lst (-> (make-list) (insert-value 10) (insert-value 20)
                (insert-value 30))]
    (is (= [10 20 30] (to-list lst))))
  (is (= [] (to-list (make-list)))))
