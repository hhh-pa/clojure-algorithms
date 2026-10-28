(ns clojure-algorithms.maths.digital-root-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.digital-root :as sut]))

(deftest test-fn-digital-root-examples
  (is (approx 0 (sut/digital-root 0)))
  (is (approx 9 (sut/digital-root 9)))
  (is (approx 2 (sut/digital-root 38)))
  (is (approx 6 (sut/digital-root 12345)))
  (is (approx 9 (sut/digital-root -45)))
  (is (approx 9 (sut/digital-root 999999999999))))

(deftest test-fn-digital-root-extra
  (is (approx 1 (sut/digital-root 10)))
  (is (thrown? Exception (sut/digital-root 3.5))))
