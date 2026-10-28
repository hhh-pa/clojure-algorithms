(ns clojure-algorithms.data-structures.linked-list.floyds-cycle-detection-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.floyds-cycle-detection
             :refer [make-list add-node link-nodes to-vec detect-cycle]]))

(deftest test-floyds
  (let [lst (make-list)]
    (is (= [] (to-vec lst)))
    (is (false? (detect-cycle lst))))
  (let [lst (add-node (make-list) 1)]
    (is (= [1] (to-vec lst)))
    (is (false? (detect-cycle lst))))
  (let [lst (reduce add-node (make-list) [1 2 3 4])]
    (is (= [1 2 3 4] (to-vec lst)))
    (is (false? (detect-cycle lst)))
    (let [lst (link-nodes lst 3 1)]
      (is (= [1 2 3 4] (to-vec lst)))
      (is (true? (detect-cycle lst)))))
  (let [lst (link-nodes (add-node (make-list) 1) 0 0)]
    (is (true? (detect-cycle lst)))))
