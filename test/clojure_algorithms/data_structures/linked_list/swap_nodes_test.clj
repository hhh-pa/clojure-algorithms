(ns clojure-algorithms.data-structures.linked-list.swap-nodes-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.swap-nodes
             :refer [make-list push swap-nodes]]))

(deftest test-swap-nodes
  (let [lst (make-list)]
    (is (= [] lst))
    (is (= [0] (push lst 0))))
  (is (= 0 (count (make-list))))
  (is (= 1 (count (push (make-list) 0))))
  (let [lst (reduce push (make-list) [5 4 3 2 1])]
    (is (= [1 2 3 4 5] lst))
    (is (= [4 2 3 1 5] (swap-nodes lst 1 4)))
    (is (= [1 2 3 4 5] (swap-nodes lst 1 1)))
    (is (= [1 2 3 4 5] (swap-nodes lst 1 99)))
    (is (= [1 2 3 4 5] (swap-nodes lst 99 98)))))
