(ns clojure-algorithms.maths.josephus-problem-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.josephus-problem :as sut]))

(deftest test-fn-josephus-recursive-examples
  (is (approx 3 (sut/josephus-recursive 7 3)))
  (is (approx 4 (sut/josephus-recursive 10 2)))
  (is (thrown? Exception (sut/josephus-recursive 0 2)))
  (is (thrown? Exception (sut/josephus-recursive 1.9 2)))
  (is (thrown? Exception (sut/josephus-recursive -2 2)))
  (is (thrown? Exception (sut/josephus-recursive 7 0)))
  (is (thrown? Exception (sut/josephus-recursive 7 -2)))
  (is (thrown? Exception (sut/josephus-recursive 1000 0.01)))
  (is (thrown? Exception (sut/josephus-recursive "cat" "dog"))))

(deftest test-fn-find-winner-examples
  (is (approx 4 (sut/find-winner 7 3)))
  (is (approx 5 (sut/find-winner 10 2))))

(deftest test-fn-josephus-iterative-examples
  (is (approx 3 (sut/josephus-iterative 5 2)))
  (is (approx 4 (sut/josephus-iterative 7 3))))
