(ns clojure-algorithms.dynamic-programming.combination-sum-iv-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.combination-sum-iv :as cs]))

(deftest combination-sum-iv-test
  (testing "all three variants agree"
    (is (= 9 (cs/combination-sum-iv [1 2 5] 5)))
    (is (= 9 (cs/combination-sum-iv-dp-array [1 2 5] 5)))
    (is (= 9 (cs/combination-sum-iv-bottom-up 3 [1 2 5] 5)))
    (is (= 1 (cs/combination-sum-iv [1 2 5] 0)))
    (is (= 1 (cs/combination-sum-iv-dp-array [1 2 5] 0)))
    (is (= 1 (cs/combination-sum-iv-bottom-up 3 [1 2 5] 0)))
    (doseq [target [1 4 7 10]]
      (is (= (cs/combination-sum-iv [1 2 3] target)
             (cs/combination-sum-iv-dp-array [1 2 3] target)
             (cs/combination-sum-iv-bottom-up 3 [1 2 3] target))))))
