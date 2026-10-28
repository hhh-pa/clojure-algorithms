(ns clojure-algorithms.maths.signum-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.signum :as sut]))

(deftest test-fn-signum-examples
  (is (approx -1 (sut/signum -10)))
  (is (approx 1 (sut/signum 10)))
  (is (approx 0 (sut/signum 0)))
  (is (approx -1 (sut/signum -20.5)))
  (is (approx 1 (sut/signum 20.5)))
  (is (approx -1 (sut/signum -1e-6)))
  (is (approx 1 (sut/signum 1e-6)))
  (is (thrown? Exception (sut/signum "Hello")))
  (is (thrown? Exception (sut/signum []))))

(deftest test-fn-test-signum-examples
  (is (nil? (sut/test-signum))))
