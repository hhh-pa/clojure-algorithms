(ns clojure-algorithms.maths.average-median-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.average-median :as sut]))

(deftest test-fn-median-examples
  (is (approx 0 (sut/median [0])))
  (is (approx 2.5 (sut/median [4 1 3 2])))
  (is (approx 8 (sut/median [2 70 6 50 20 8 4]))))
