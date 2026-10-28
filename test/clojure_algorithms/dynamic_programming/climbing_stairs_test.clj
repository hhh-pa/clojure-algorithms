(ns clojure-algorithms.dynamic-programming.climbing-stairs-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.climbing-stairs :as cs]))

(deftest climb-stairs-test
  (testing "ways to climb"
    (is (= 3 (cs/climb-stairs 3)))
    (is (= 1 (cs/climb-stairs 1)))
    (is (= 2 (cs/climb-stairs 2)))
    (is (= 89 (cs/climb-stairs 10)))
    (is (thrown? AssertionError (cs/climb-stairs -7)))
    (is (thrown? AssertionError (cs/climb-stairs 0)))))
