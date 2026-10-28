(ns clojure-algorithms.graphs.bidirectional-breadth-first-search-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.bidirectional-breadth-first-search
             :refer [bfs-search bidirectional-search]]))

(def grid
  [[0 0 0 0 0 0 0] [0 1 0 0 0 0 0] [0 0 0 0 0 0 0]
   [0 0 1 0 0 0 0] [1 0 1 0 0 0 0] [0 0 0 0 0 0 0]
   [0 0 0 0 1 0 0]])

(defn- valid-path? [grid path start goal]
  (and (= start (first path)) (= goal (last path))
       (every? #(zero? (get-in grid %)) path)
       (every? (fn [[[y1 x1] [y2 x2]]]
                 (= 1 (+ (Math/abs (- y1 y2)) (Math/abs (- x1 x2)))))
               (map vector path (rest path)))))

(deftest test-bidirectional-bfs
  (let [p (bfs-search grid [0 0] [6 6])]
    (is (= 13 (count p)))
    (is (valid-path? grid p [0 0] [6 6])))
  (let [p (bidirectional-search grid [0 0] [6 6])]
    (is (= 13 (count p)))
    (is (valid-path? grid p [0 0] [6 6])))
  (is (= [[0 0]] (bfs-search [[0 1] [1 0]] [0 0] [1 1])))
  (is (= [[0 0]] (bidirectional-search [[0 1] [1 0]] [0 0] [1 1])))
  (is (= [[2 2]] (bidirectional-search grid [2 2] [2 2]))))
