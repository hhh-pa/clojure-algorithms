(ns clojure-algorithms.graphs.depth-first-search-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.depth-first-search
             :refer [depth-first-search]]))

(def demo {"A" ["B" "C" "D"] "B" ["A" "D" "E"] "C" ["A" "F"]
           "D" ["B" "D"] "E" ["B" "F"] "F" ["C" "E" "G"] "G" ["F"]})

(deftest test-depth-first-search
  (is (= #{"A" "B" "C" "D" "E" "F" "G"} (depth-first-search demo "A")))
  (is (= #{"A" "B" "C" "D" "E" "F" "G"} (depth-first-search demo "G")))
  (is (= #{"D"} (depth-first-search {"D" []} "D"))))
