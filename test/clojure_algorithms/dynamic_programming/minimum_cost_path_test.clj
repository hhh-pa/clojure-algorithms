(ns clojure-algorithms.dynamic-programming.minimum-cost-path-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-cost-path :as mc]))

(deftest minimum-cost-path-test
  (testing "minimum path costs"
    (is (= 6 (mc/minimum-cost-path [[2 1] [3 1] [4 2]])))
    (is (= 7 (mc/minimum-cost-path [[2 1 4] [2 1 3] [3 2 1]])))
    (is (= 5 (mc/minimum-cost-path [[5]])))
    (is (= 12 (mc/minimum-cost-path [[1 2 3] [4 5 6]])))))
