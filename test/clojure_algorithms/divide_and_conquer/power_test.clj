(ns clojure-algorithms.divide-and-conquer.power-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.power :as p]))

(deftest actual-power-test
  (testing "non-negative exponents"
    (is (= 9 (p/actual-power 3 2)))
    (is (= 125 (p/actual-power 5 3)))
    (is (= 32 (p/actual-power 2 5)))
    (is (= 1 (p/actual-power 7 0)))))

(deftest power-test
  (testing "positive and negative exponents"
    (is (= 4096 (p/power 4 6)))
    (is (= 8 (p/power 2 3)))
    (is (= -8 (p/power -2 3)))
    (is (= 1/8 (p/power 2 -3)))
    (is (= -1/8 (p/power -2 -3)))))
