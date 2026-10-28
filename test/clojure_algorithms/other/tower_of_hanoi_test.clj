(ns clojure-algorithms.other.tower-of-hanoi-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.tower-of-hanoi :as h]))

(deftest moves-test
  (testing "doctest trace for height 3"
    (is (= [["A" "B"] ["A" "C"] ["B" "C"] ["A" "B"]
            ["C" "A"] ["C" "B"] ["A" "B"]]
           (h/moves 3 "A" "B" "C"))))
  (testing "move counts are 2^n - 1"
    (is (= 1 (count (h/moves 1 "A" "B" "C"))))
    (is (= 15 (count (h/moves 4 "A" "B" "C")))))
  (testing "zero height has no moves"
    (is (= [] (h/moves 0 "A" "B" "C")))))
