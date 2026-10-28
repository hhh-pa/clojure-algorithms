(ns clojure-algorithms.maths.special-numbers.deficient-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.deficient-numbers :as sut]))

(deftest test-fn-is-deficient-number-examples
  (is (= false (boolean (sut/is-deficient-number -1))))
  (is (= false (boolean (sut/is-deficient-number 0))))
  (is (= true (boolean (sut/is-deficient-number 1))))
  (is (= true (boolean (sut/is-deficient-number 2))))
  (is (= false (boolean (sut/is-deficient-number 6))))
  (is (= false (boolean (sut/is-deficient-number 12))))
  (is (= true (boolean (sut/is-deficient-number 7))))
  (is (= false (boolean (sut/is-deficient-number 28))))
  (is (= true (boolean (sut/is-deficient-number 15))))
  (is (thrown? Exception (sut/is-deficient-number 8.0))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-deficient-number 8.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
