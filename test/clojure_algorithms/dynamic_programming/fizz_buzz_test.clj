(ns clojure-algorithms.dynamic-programming.fizz-buzz-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.fizz-buzz :as fb]))

(deftest fizz-buzz-test
  (testing "FizzBuzz output"
    (is (= "1 2 Fizz 4 Buzz Fizz 7 " (fb/fizz-buzz 1 7)))
    (is (= "Buzz " (fb/fizz-buzz 5 5)))
    (is (= "FizzBuzz " (fb/fizz-buzz 15 15))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (fb/fizz-buzz 1 0)))
    (is (thrown? IllegalArgumentException (fb/fizz-buzz -5 5)))
    (is (thrown? IllegalArgumentException (fb/fizz-buzz 10 -5)))
    (is (thrown? IllegalArgumentException (fb/fizz-buzz 1.5 5)))
    (is (thrown? IllegalArgumentException (fb/fizz-buzz 1 5.5)))))
