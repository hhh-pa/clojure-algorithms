(ns clojure-algorithms.dynamic-programming.palindrome-partitioning-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.palindrome-partitioning :as pp]))

(deftest palindrome-partitioning-test
  (testing "minimum palindrome cuts"
    (is (= 1 (pp/find-minimum-partitions "aab")))
    (is (= 0 (pp/find-minimum-partitions "aaa")))
    (is (= 3 (pp/find-minimum-partitions "ababbbabbababa")))
    (is (= 0 (pp/find-minimum-partitions "racecar")))
    (is (= 0 (pp/find-minimum-partitions "a")))))
