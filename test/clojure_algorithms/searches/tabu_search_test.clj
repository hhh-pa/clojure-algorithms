(ns clojure-algorithms.searches.tabu-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.tabu-search :as tabu]))

(def tiny-graph
  "a b 1\na c 5\na d 6\nb c 2\nb d 7\nc d 3")

(deftest generate-neighbours-test
  (testing "string content parses to a mirrored neighbour map"
    (let [d (tabu/generate-neighbours tiny-graph)]
      (is (= [["b" 1] ["c" 5] ["d" 6]] (d "a")))
      (is (= [["a" 1] ["c" 2] ["d" 7]] (d "b")))
      (is (= ["a" "b" "c" "d"] (sort (keys d))))))
  (testing "collection of lines works too"
    (let [d (tabu/generate-neighbours ["a b 1" "b c 2"])]
      (is (= [["b" 1]] (d "a")))
      (is (= [["a" 1] ["c" 2]] (d "b"))))))

(deftest generate-first-solution-test
  (testing "greedy tour from a"
    (let [d (tabu/generate-neighbours tiny-graph)]
      (is (= [["a" "b" "c" "d" "a"] 12]
             (tabu/generate-first-solution "a" d))))))

(deftest find-neighborhood-test
  (testing "1-1 swap neighbourhood sorted by distance"
    (let [d {"a" [["b" 20] ["c" 18] ["d" 22] ["e" 26]]
             "c" [["a" 18] ["b" 10] ["d" 23] ["e" 24]]
             "b" [["a" 20] ["c" 10] ["d" 11] ["e" 12]]
             "e" [["a" 26] ["b" 12] ["c" 24] ["d" 40]]
             "d" [["a" 22] ["b" 11] ["c" 23] ["e" 40]]}]
      (is (= [["a" "e" "b" "d" "c" "a" 90]
              ["a" "c" "d" "b" "e" "a" 90]
              ["a" "d" "b" "c" "e" "a" 93]
              ["a" "c" "b" "e" "d" "a" 102]
              ["a" "c" "e" "d" "b" "a" 113]
              ["a" "b" "c" "d" "e" "a" 119]]
             (tabu/find-neighborhood ["a" "c" "b" "d" "e" "a"] d))))))

(deftest tabu-search-test
  (testing "search never regresses past the first solution"
    (let [d (tabu/generate-neighbours tiny-graph)
          [first-tour first-dist] (tabu/generate-first-solution "a" d)
          [best cost] (tabu/tabu-search first-tour first-dist d 2 3)]
      (is (<= cost first-dist))
      (is (= "a" (first best) (peek best)))
      (is (= #{"a" "b" "c" "d"} (set best))))))
