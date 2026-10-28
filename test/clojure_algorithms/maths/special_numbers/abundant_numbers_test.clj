(ns clojure-algorithms.maths.special-numbers.abundant-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.abundant-numbers :as sut]))

(deftest test-fn-is-abundant-number-examples
  (is (= false (boolean (sut/is-abundant-number -1))))
  (is (= false (boolean (sut/is-abundant-number 0))))
  (is (= true (boolean (sut/is-abundant-number 12))))
  (is (= true (boolean (sut/is-abundant-number 18))))
  (is (= false (boolean (sut/is-abundant-number 28))))
  (is (= true (boolean (sut/is-abundant-number 20))))
  (is (= false (boolean (sut/is-abundant-number 6))))
  (is (= false (boolean (sut/is-abundant-number 1))))
  (is (= true (boolean (sut/is-abundant-number 945))))
  (is (thrown? Exception (sut/is-abundant-number 28.0))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-abundant-number 28.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
