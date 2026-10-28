(ns clojure-algorithms.graphs.breadth-first-search-2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.breadth-first-search-2
             :refer [breadth-first-search breadth-first-search-with-deque]]))

(def demo {"A" ["B" "C"] "B" ["D" "E"] "C" ["F"] "D" [] "E" ["F"] "F" []})

(deftest test-breadth-first-search-2
  (is (= ["A" "B" "C" "D" "E" "F"] (breadth-first-search demo "A")))
  (is (= ["A" "B" "C" "D" "E" "F"]
         (breadth-first-search-with-deque demo "A")))
  (is (= ["F"] (breadth-first-search demo "F"))))
