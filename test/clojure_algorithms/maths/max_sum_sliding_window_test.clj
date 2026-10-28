(ns clojure-algorithms.maths.max-sum-sliding-window-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.max-sum-sliding-window :as sut]))

(deftest test-fn-max-sum-in-array-examples
  (is (approx 24 (let [arr [1 4 2 10 2 3 1 0 20] k 4] (sut/max-sum-in-array arr k))))
  (is (thrown? Exception (let [arr [1 4 2 10 2 3 1 0 20] k 10] (sut/max-sum-in-array arr k))))
  (is (approx 27 (let [arr [1 4 2 10 2 13 1 0 2] k 4] (sut/max-sum-in-array arr k)))))
