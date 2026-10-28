(ns clojure-algorithms.dynamic-programming.minimum-squares-to-represent-a-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-squares-to-represent-a-number :as ms]))

(deftest min-squares-test
  (testing "minimum square counts"
    (is (= 1 (ms/minimum-squares-to-represent-a-number 25)))
    (is (= 2 (ms/minimum-squares-to-represent-a-number 37)))
    (is (= 3 (ms/minimum-squares-to-represent-a-number 21)))
    (is (= 2 (ms/minimum-squares-to-represent-a-number 58)))
    (is (= 1 (ms/minimum-squares-to-represent-a-number 0)))
    (is (= 4 (ms/minimum-squares-to-represent-a-number 7))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (ms/minimum-squares-to-represent-a-number -1)))
    (is (thrown? IllegalArgumentException
                 (ms/minimum-squares-to-represent-a-number 12.34)))))
