(ns clojure-algorithms.maths.perfect-cube-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.perfect-cube :as sut]))

(deftest test-fn-perfect-cube-examples
  (is (= true (boolean (sut/perfect-cube 27))))
  (is (= false (boolean (sut/perfect-cube 4)))))

(deftest test-fn-perfect-cube-binary-search-examples
  (is (= true (boolean (sut/perfect-cube-binary-search 27))))
  (is (= true (boolean (sut/perfect-cube-binary-search 64))))
  (is (= false (boolean (sut/perfect-cube-binary-search 4))))
  (is (thrown? Exception (sut/perfect-cube-binary-search "a")))
  (is (thrown? Exception (sut/perfect-cube-binary-search 0.1))))
