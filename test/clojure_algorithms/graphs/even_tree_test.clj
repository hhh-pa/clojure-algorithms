(ns clojure-algorithms.graphs.even-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.even-tree :refer [even-tree-cuts]]))

(deftest test-even-tree
  (let [tree (reduce (fn [t [u v]]
                       (-> t (update u (fnil conj []) v)
                           (update v (fnil conj []) u)))
                     {} [[2 1] [3 1] [4 3] [5 2] [6 1] [7 2] [8 6] [9 8]
                         [10 8]])]
    (is (= 2 (even-tree-cuts tree))))
  (is (= 0 (even-tree-cuts {1 [2] 2 [1]})))
  (is (= 1 (even-tree-cuts {1 [2 3] 2 [1 4] 3 [1] 4 [2]}))))
