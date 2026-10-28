(ns clojure-algorithms.dynamic-programming.minimum-steps-to-one-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-steps-to-one :as ms]))

(deftest min-steps-test
  (testing "minimum steps to one"
    (is (= 3 (ms/min-steps-to-one 10)))
    (is (= 4 (ms/min-steps-to-one 15)))
    (is (= 2 (ms/min-steps-to-one 6)))
    (is (= 0 (ms/min-steps-to-one 1)))
    (is (= 1 (ms/min-steps-to-one 2))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ms/min-steps-to-one 0)))
    (is (thrown? IllegalArgumentException (ms/min-steps-to-one -3)))))
