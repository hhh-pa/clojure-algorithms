(ns clojure-algorithms.maths.ceil-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.ceil :as sut]))

(deftest test-fn-ceil-examples
  (is (= 1 (sut/ceil 1)))
  (is (= -1 (sut/ceil -1)))
  (is (= 0 (sut/ceil 0)))
  (is (= 2 (sut/ceil 1.1)))
  (is (= -1 (sut/ceil -1.1)))
  (is (= 1 (sut/ceil 1.0)))
  (is (= -1 (sut/ceil -1.0)))
  (is (= 1000000000 (sut/ceil 1000000000)))
  (is (true? (every? true?
                     (map (fn [[in expected]]
                            (= expected (long (sut/ceil in))))
                          [[1 1] [-1 -1] [0 0] [1.1 2] [-1.1 -1]
                           [1.0 1] [-1.0 -1] [1000000000 1000000000]])))))

(deftest test-ceil-extra
  (is (= 0 (sut/ceil 0.0)))
  (is (= 0 (sut/ceil -0.5)))
  (is (= 4 (sut/ceil 7/2)))
  (is (= 5 (sut/ceil 5N))))
