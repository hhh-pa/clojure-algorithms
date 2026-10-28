(ns clojure-algorithms.maths.find-min-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.find-min :as sut]))

(deftest test-fn-find-min-iterative-examples
  (is (approx -56 (sut/find-min-iterative [0 1 2 3 4 5 -3 24 -56])))
  (is (thrown? Exception (sut/find-min-iterative []))))

(deftest test-fn-find-min-recursive-examples
  (is (thrown? Exception (sut/find-min-recursive [] 0 0))))
