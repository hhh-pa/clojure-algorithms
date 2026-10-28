(ns clojure-algorithms.maths.special-numbers.carmichael-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.carmichael-number :as sut]))

(deftest test-fn-power-examples
  (is (approx 2 (sut/power 2 15 3)))
  (is (approx 5 (sut/power 5 1 30)))
  (is (approx 1 (sut/power 7 0 5))))

(deftest test-fn-is-carmichael-number-examples
  (is (false? (sut/is-carmichael-number 4)))
  (is (true? (sut/is-carmichael-number 561)))
  (is (false? (sut/is-carmichael-number 562)))
  (is (false? (sut/is-carmichael-number 900)))
  (is (true? (sut/is-carmichael-number 1105)))
  (is (true? (sut/is-carmichael-number 8911)))
  (is (thrown? Exception (sut/is-carmichael-number 5.1)))
  (is (thrown? Exception (sut/is-carmichael-number -7)))
  (is (thrown? Exception (sut/is-carmichael-number 0))))

(deftest test-error-type
  (is (= :ValueError
         (try (sut/is-carmichael-number 0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))

(deftest test-vars-exist
  (is (some? #'sut/power))
  (is (some? #'sut/is-carmichael-number)))
