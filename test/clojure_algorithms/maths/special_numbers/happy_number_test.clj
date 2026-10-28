(ns clojure-algorithms.maths.special-numbers.happy-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.happy-number :as sut]))

(deftest test-fn-is-happy-number-examples
  (is (true? (sut/is-happy-number 19)))
  (is (false? (sut/is-happy-number 2)))
  (is (true? (sut/is-happy-number 23)))
  (is (true? (sut/is-happy-number 1)))
  (is (true? (sut/is-happy-number 7)))
  (is (false? (sut/is-happy-number 4)))
  (is (thrown? Exception (sut/is-happy-number 0)))
  (is (thrown? Exception (sut/is-happy-number -19)))
  (is (thrown? Exception (sut/is-happy-number 19.1)))
  (is (thrown? Exception (sut/is-happy-number "happy"))))

(deftest test-error-details
  (let [thrown (try (sut/is-happy-number 0)
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (= :ValueError (:type (ex-data thrown))))
    (is (= "number=0 must be a positive integer" (ex-message thrown))))
  (let [thrown (try (sut/is-happy-number "happy")
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (= "number='happy' must be a positive integer" (ex-message thrown)))))

(deftest test-vars-exist
  (is (some? #'sut/is-happy-number)))
