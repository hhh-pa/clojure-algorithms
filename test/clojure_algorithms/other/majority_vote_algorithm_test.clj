(ns clojure-algorithms.other.majority-vote-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.majority-vote-algorithm :as m]))

(deftest majority-vote-test
  (testing "doctest examples (order-insensitive)"
    (is (= #{2} (set (m/majority-vote [1 2 2 3 1 3 2] 3))))
    (is (= #{} (set (m/majority-vote [1 2 2 3 1 3 2] 2))))
    (is (= #{1 2 3} (set (m/majority-vote [1 2 2 3 1 3 2] 4)))))
  (testing "single dominant element"
    (is (= [5] (m/majority-vote [5 5 5 1 2] 2)))))
