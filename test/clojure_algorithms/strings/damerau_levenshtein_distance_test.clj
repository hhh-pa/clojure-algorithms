(ns clojure-algorithms.strings.damerau-levenshtein-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.damerau-levenshtein-distance
             :refer [damerau-levenshtein-distance]]))

(deftest test-damerau-levenshtein-distance
  (testing "doctest examples"
    (is (= 1 (damerau-levenshtein-distance "cat" "cut")))
    (is (= 3 (damerau-levenshtein-distance "kitten" "sitting")))
    (is (= 4 (damerau-levenshtein-distance "hello" "world")))
    (is (= 2 (damerau-levenshtein-distance "book" "back")))
    (is (= 3 (damerau-levenshtein-distance "container" "containment"))))
  (testing "edges"
    (is (= 0 (damerau-levenshtein-distance "" "")))
    (is (= 3 (damerau-levenshtein-distance "" "abc")))
    (is (= 1 (damerau-levenshtein-distance "ab" "ba")))))
