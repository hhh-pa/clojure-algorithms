(ns clojure-algorithms.graphs.multi-heuristic-astar-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.multi-heuristic-astar
             :refer [multi-a-star make-pq]]))

(deftest test-multi-heuristic-astar
  (let [path (multi-a-star [0 0] [4 4] 5 #{})]
    (is (= [0 0] (first path)))
    (is (= [4 4] (last path)))
    (is (= 9 (count path)))
    (doseq [[[x1 y1] [x2 y2]] (map vector path (rest path))]
      (is (= 1 (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))))
  (is (nil? (multi-a-star [0 0] [1 1] 2 #{[0 1] [1 0]})))
  (let [pq (make-pq)]
    (is (= Double/POSITIVE_INFINITY
           (do (reset! pq {:heap (sorted-set) :prio {}})
               (if (empty? (:heap @pq)) Double/POSITIVE_INFINITY 0))))))
