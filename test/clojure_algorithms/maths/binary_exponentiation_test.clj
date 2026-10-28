(ns clojure-algorithms.maths.binary-exponentiation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.binary-exponentiation :as sut]))

(deftest test-fn-binary-exp-recursive-examples
  (is (approx 243 (sut/binary-exp-recursive 3 5)))
  (is (approx 34522712143931 (sut/binary-exp-recursive 11 13)))
  (is (approx -1 (sut/binary-exp-recursive -1 3)))
  (is (approx 0 (sut/binary-exp-recursive 0 5)))
  (is (approx 3 (sut/binary-exp-recursive 3 1)))
  (is (approx 1 (sut/binary-exp-recursive 3 0)))
  (is (approx 5.0625 (sut/binary-exp-recursive 1.5 4)))
  (is (thrown? Exception (sut/binary-exp-recursive 3 -1))))

(deftest test-fn-binary-exp-iterative-examples
  (is (approx 243 (sut/binary-exp-iterative 3 5)))
  (is (approx 34522712143931 (sut/binary-exp-iterative 11 13)))
  (is (approx -1 (sut/binary-exp-iterative -1 3)))
  (is (approx 0 (sut/binary-exp-iterative 0 5)))
  (is (approx 3 (sut/binary-exp-iterative 3 1)))
  (is (approx 1 (sut/binary-exp-iterative 3 0)))
  (is (approx 5.0625 (sut/binary-exp-iterative 1.5 4)))
  (is (thrown? Exception (sut/binary-exp-iterative 3 -1))))

(deftest test-fn-binary-exp-mod-recursive-examples
  (is (approx 1 (sut/binary-exp-mod-recursive 3 4 5)))
  (is (approx 4 (sut/binary-exp-mod-recursive 11 13 7)))
  (is (approx 2.0625 (sut/binary-exp-mod-recursive 1.5 4 3)))
  (is (thrown? Exception (sut/binary-exp-mod-recursive 7 -1 10)))
  (is (thrown? Exception (sut/binary-exp-mod-recursive 7 13 0))))

(deftest test-fn-binary-exp-mod-iterative-examples
  (is (approx 1 (sut/binary-exp-mod-iterative 3 4 5)))
  (is (approx 4 (sut/binary-exp-mod-iterative 11 13 7)))
  (is (approx 2.0625 (sut/binary-exp-mod-iterative 1.5 4 3)))
  (is (thrown? Exception (sut/binary-exp-mod-iterative 7 -1 10)))
  (is (thrown? Exception (sut/binary-exp-mod-iterative 7 13 0))))
