(ns clojure-algorithms.graphs.g-topological-sort-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.g-topological-sort
             :refer [topological-sort]]))

(def clothes [[1 4] [2 4] [3] [] [] [4] [2 7] [3] []])

(deftest test-g-topological-sort
  (let [order (topological-sort clothes)
        pos (into {} (map-indexed (fn [i v] [v i]) order))]
    (is (= (set (range 9)) (set order)))
    (doseq [u (range 9) v (nth clothes u)]
      (is (< (get pos u) (get pos v))))))
