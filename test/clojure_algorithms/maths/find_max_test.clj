(ns clojure-algorithms.maths.find-max-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.find-max :as sut]))

(deftest test-fn-find-max-iterative-examples
  (is (approx 94 (sut/find-max-iterative [2 4 9 7 19 94 5])))
  (is (thrown? Exception (sut/find-max-iterative []))))

(deftest test-fn-find-max-recursive-examples
  (is (thrown? Exception (sut/find-max-recursive [] 0 0))))
