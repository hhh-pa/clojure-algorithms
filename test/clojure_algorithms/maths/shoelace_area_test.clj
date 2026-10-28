(ns clojure-algorithms.maths.shoelace-area-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.shoelace-area :as sut]))

(deftest test-fn-shoelace-area-examples
  (is (approx 6.0 (sut/shoelace-area [[0 0] [4 0] [0 3]])))
  (is (approx 1.0 (sut/shoelace-area [[0 0] [1 0] [1 1] [0 1]])))
  (is (approx 4.0 (sut/shoelace-area [[0 0] [2 0] [2 2] [0 2]]))))
