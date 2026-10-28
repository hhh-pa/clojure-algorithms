(ns clojure-algorithms.matrix.matrix-multiplication-recursion-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.matrix-multiplication-recursion :as sut]))

(deftest test-fn-is-square-examples
  (is (= true (boolean (sut/is-square []))))
  (is (= true (boolean (sut/is-square sut/matrix-1-to-4))))
  (is (= false (boolean (sut/is-square sut/matrix-5-to-9-high)))))

(deftest test-fn-matrix-multiply-examples
  (is (approx [[19 22] [43 50]] (sut/matrix-multiply sut/matrix-1-to-4 sut/matrix-5-to-8))))

(deftest test-fn-matrix-multiply-recursive-examples
  (is (approx [] (sut/matrix-multiply-recursive [] [])))
  (is (thrown? Exception (sut/matrix-multiply-recursive sut/matrix-1-to-4 sut/matrix-5-to-9-wide)))
  (is (thrown? Exception (sut/matrix-multiply-recursive sut/matrix-1-to-4 sut/matrix-5-to-9-high)))
  (is (thrown? Exception (sut/matrix-multiply-recursive sut/matrix-1-to-4 sut/matrix-count-up))))
