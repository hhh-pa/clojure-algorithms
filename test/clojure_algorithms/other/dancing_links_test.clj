(ns clojure-algorithms.other.dancing-links-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.dancing-links :as d]))

(deftest solve-test
  (testing "doctest instance has no exact cover (matches Python)"
    (is (= [] (d/solve [1 2 3 4 5 6 7]
                       [[1 4 7] [1 4] [4 5 7] [3 5 6] [2 3 6 7]]))))
  (testing "adding [2 7] yields the Wikipedia cover"
    (let [sols (d/solve [1 2 3 4 5 6 7]
                        [[1 4 7] [1 4] [4 5 7] [3 5 6] [2 3 6 7] [2 7]])]
      (is (= 1 (count sols)))
      (is (= #{1 2 3 4 5 6 7} (set (mapcat seq (first sols)))))))
  (testing "trivial single cover"
    (is (= [[#{1 2}]] (d/solve [1 2] [[1 2]])))))
