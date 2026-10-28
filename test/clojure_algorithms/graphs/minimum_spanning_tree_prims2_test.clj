(ns clojure-algorithms.graphs.minimum-spanning-tree-prims2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.minimum-spanning-tree-prims2
             :refer [make-graph add-edge prims-algo]]))

(deftest test-prims-algo
  (let [g (-> (make-graph)
              (add-edge "a" "b" 3) (add-edge "b" "c" 10)
              (add-edge "c" "d" 5) (add-edge "a" "c" 15)
              (add-edge "b" "d" 100))
        [dist _] (prims-algo g)]
    (is (= 3 (Math/abs (- (get dist "a") (get dist "b")))))
    (is (= 15 (Math/abs (- (get dist "d") (get dist "b")))))
    (is (= 13 (Math/abs (- (get dist "a") (get dist "c"))))))
  (is (= [{} {}] (prims-algo (make-graph)))))
