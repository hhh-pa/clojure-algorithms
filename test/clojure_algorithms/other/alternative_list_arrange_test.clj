(ns clojure-algorithms.other.alternative-list-arrange-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.alternative-list-arrange :as a]))

(deftest alternative-list-arrange-test
  (testing "first longer"
    (is (= [1 "A" 2 "B" 3 "C" 4 5]
           (a/alternative-list-arrange [1 2 3 4 5] ["A" "B" "C"]))))
  (testing "second longer"
    (is (= ["A" 1 "B" 2 "C" 3 4 5]
           (a/alternative-list-arrange ["A" "B" "C"] [1 2 3 4 5]))))
  (testing "equal-ish lengths"
    (is (= ["X" 9 "Y" 8 "Z" 7 6]
           (a/alternative-list-arrange ["X" "Y" "Z"] [9 8 7 6]))))
  (testing "empty second"
    (is (= [1 2 3 4 5] (a/alternative-list-arrange [1 2 3 4 5] []))))
  (testing "both empty"
    (is (= [] (a/alternative-list-arrange [] [])))))
