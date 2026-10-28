(ns clojure-algorithms.maths.remove-digit-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.remove-digit :as sut]))

(deftest test-fn-remove-digit-examples
  (is (approx 52 (sut/remove-digit 152)))
  (is (approx 685 (sut/remove-digit 6385)))
  (is (approx 1 (sut/remove-digit -11)))
  (is (approx 222222 (sut/remove-digit 2222222)))
  (is (thrown? Exception (sut/remove-digit "2222222")))
  (is (thrown? Exception (sut/remove-digit "string input"))))
