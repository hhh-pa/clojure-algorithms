(ns clojure-algorithms.searches.median-of-medians-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.median-of-medians :as mom]))

(deftest median-of-five-test
  (testing "small medians"
    (is (= 5 (mom/median-of-five [2 4 5 7 899])))
    (is (= 32 (mom/median-of-five [5 7 899 54 32])))
    (is (= 4 (mom/median-of-five [5 4 3 2])))
    (is (= 5 (mom/median-of-five [3 5 7 10 2])))))

(deftest median-of-medians-test
  (testing "median of medians pivots"
    (is (= 54 (mom/median-of-medians [2 4 5 7 899 54 32])))
    (is (= 32 (mom/median-of-medians [5 7 899 54 32])))
    (is (= 4 (mom/median-of-medians [5 4 3 2])))
    (is (= 12 (mom/median-of-medians [3 5 7 10 2 12])))))

(deftest quick-select-test
  (testing "1-based rank selection"
    (is (= 32 (mom/quick-select [2 4 5 7 899 54 32] 5)))
    (is (= 2 (mom/quick-select [2 4 5 7 899 54 32] 1)))
    (is (= 3 (mom/quick-select [5 4 3 2] 2)))
    (is (= 5 (mom/quick-select [3 5 7 10 2 12] 3)))
    (is (= -1 (mom/quick-select [1 2] 5)))))
