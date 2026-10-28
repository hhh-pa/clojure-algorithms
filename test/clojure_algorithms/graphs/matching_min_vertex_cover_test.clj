(ns clojure-algorithms.graphs.matching-min-vertex-cover-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.matching-min-vertex-cover
             :refer [get-edges matching-min-vertex-cover]]))

(deftest test-matching-min-vertex-cover
  (is (= #{[0 1] [1 0] [0 3] [3 0] [3 1] [1 3] [2 0] [2 3] [3 2]}
         (get-edges {0 [1 3] 1 [0 3] 2 [0 3] 3 [0 1 2]})))
  (let [g {0 [1 3] 1 [0 3] 2 [0 3 4] 3 [0 1 2] 4 [2 3]}
        cover (matching-min-vertex-cover g)]
    (doseq [[a b] (get-edges g)]
      (is (or (contains? cover a) (contains? cover b))))))
