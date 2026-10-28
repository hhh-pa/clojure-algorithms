(ns clojure-algorithms.maths.number-of-digits-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.number-of-digits :as sut]))

(deftest test-fn-num-digits-examples
  (is (approx 5 (sut/num-digits 12345)))
  (is (approx 3 (sut/num-digits 123)))
  (is (approx 1 (sut/num-digits 0)))
  (is (approx 1 (sut/num-digits -1)))
  (is (approx 6 (sut/num-digits -123456)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/num-digits "123"))))

(deftest test-fn-num-digits-fast-examples
  (is (approx 5 (sut/num-digits-fast 12345)))
  (is (approx 3 (sut/num-digits-fast 123)))
  (is (approx 4 (sut/num-digits-fast 1000)))
  (is (approx 16 (sut/num-digits-fast 1000000000000000)))
  (is (approx 1 (sut/num-digits-fast 0)))
  (is (approx 1 (sut/num-digits-fast -1)))
  (is (approx 6 (sut/num-digits-fast -123456)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/num-digits-fast "123"))))

(deftest test-fn-num-digits-faster-examples
  (is (approx 5 (sut/num-digits-faster 12345)))
  (is (approx 3 (sut/num-digits-faster 123)))
  (is (approx 1 (sut/num-digits-faster 0)))
  (is (approx 1 (sut/num-digits-faster -1)))
  (is (approx 6 (sut/num-digits-faster -123456)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/num-digits-faster "123"))))

(deftest test-fn-benchmark-stub
  (is (thrown? clojure.lang.ExceptionInfo (sut/benchmark))))
