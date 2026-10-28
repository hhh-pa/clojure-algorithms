(ns clojure-algorithms.data-structures.linked-list.has-loop-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.has-loop
             :refer [make-list add-node link-nodes to-vec has-loop?]]))

(deftest test-has-loop
  (let [lst (reduce add-node (make-list) [1 2 3 4])]
    (is (false? (has-loop? lst)))
    (is (= [1 2 3 4] (to-vec lst)))
    (let [lst (link-nodes lst 3 1)]
      (is (true? (has-loop? lst)))
      (is (thrown? clojure.lang.ExceptionInfo (to-vec lst)))))
  (is (false? (has-loop? (reduce add-node (make-list) [5 6 5 6]))))
  (is (false? (has-loop? (add-node (make-list) 1))))
  (is (false? (has-loop? (make-list)))))
