(ns clojure-algorithms.maths.special-numbers.disarum-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.disarum-number :as sut]))

(deftest test-fn-is-disarium-number-examples
  (is (= false (boolean (sut/is-disarium-number -1))))
  (is (= false (boolean (sut/is-disarium-number 0))))
  (is (= true (boolean (sut/is-disarium-number 1))))
  (is (= true (boolean (sut/is-disarium-number 89))))
  (is (= true (boolean (sut/is-disarium-number 135))))
  (is (= true (boolean (sut/is-disarium-number 175))))
  (is (= true (boolean (sut/is-disarium-number 518))))
  (is (= true (boolean (sut/is-disarium-number 598))))
  (is (= false (boolean (sut/is-disarium-number 123))))
  (is (thrown? Exception (sut/is-disarium-number 89.0))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-disarium-number 89.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
