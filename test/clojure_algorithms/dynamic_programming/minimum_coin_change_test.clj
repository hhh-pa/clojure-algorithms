(ns clojure-algorithms.dynamic-programming.minimum-coin-change-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-coin-change :as mc]))

(deftest dp-count-test
  (testing "coin change counts"
    (is (= 4 (mc/dp-count [1 2 3] 4)))
    (is (= 8 (mc/dp-count [1 2 3] 7)))
    (is (= 5 (mc/dp-count [2 5 3 6] 10)))
    (is (= 0 (mc/dp-count [10] 99)))
    (is (= 1 (mc/dp-count [4 5 6] 0)))
    (is (= 0 (mc/dp-count [1 2 3] -5)))))
