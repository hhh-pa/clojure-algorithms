(ns clojure-algorithms.maths.perfect-square-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.perfect-square :as sut]))

(deftest test-fn-perfect-square-examples
  (is (= true (boolean (sut/perfect-square 9))))
  (is (= true (boolean (sut/perfect-square 16))))
  (is (= true (boolean (sut/perfect-square 1))))
  (is (= true (boolean (sut/perfect-square 0))))
  (is (= false (boolean (sut/perfect-square 10)))))

(deftest test-fn-perfect-square-binary-search-examples
  (is (= true (boolean (sut/perfect-square-binary-search 9))))
  (is (= true (boolean (sut/perfect-square-binary-search 16))))
  (is (= true (boolean (sut/perfect-square-binary-search 1))))
  (is (= true (boolean (sut/perfect-square-binary-search 0))))
  (is (= false (boolean (sut/perfect-square-binary-search 10))))
  (is (= false (boolean (sut/perfect-square-binary-search -1))))
  (is (= false (boolean (sut/perfect-square-binary-search 1.1))))
  (is (thrown? Exception (sut/perfect-square-binary-search "a")))
  (is (thrown? Exception (sut/perfect-square-binary-search []))))
