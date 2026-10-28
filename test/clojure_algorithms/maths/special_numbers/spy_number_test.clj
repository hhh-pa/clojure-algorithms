(ns clojure-algorithms.maths.special-numbers.spy-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.spy-number :as sut]))

(deftest test-fn-is-spy-number-examples
  (is (false? (sut/is-spy-number -1)))
  (is (false? (sut/is-spy-number 0)))
  (is (true? (sut/is-spy-number 22)))
  (is (true? (sut/is-spy-number 1124)))
  (is (false? (sut/is-spy-number 124)))
  (is (true? (sut/is-spy-number 123)))
  (is (false? (sut/is-spy-number 125)))
  (is (thrown? Exception (sut/is-spy-number 5.0))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/is-spy-number 5.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))

(deftest test-vars-exist
  (is (some? #'sut/is-spy-number)))
