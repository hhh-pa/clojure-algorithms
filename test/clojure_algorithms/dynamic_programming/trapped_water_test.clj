(ns clojure-algorithms.dynamic-programming.trapped-water-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.trapped-water :as tw]))

(deftest trapped-water-test
  (testing "trapped rainwater"
    (is (= 6 (tw/trapped-rainwater [0 1 0 2 1 0 1 3 2 1 2 1])))
    (is (= 9 (tw/trapped-rainwater [7 1 5 3 6 4])))
    (is (= 0 (tw/trapped-rainwater [])))
    (is (= 0 (tw/trapped-rainwater [1 2 3])))
    (is (thrown? IllegalArgumentException
                 (tw/trapped-rainwater [7 1 5 3 6 -1])))))
