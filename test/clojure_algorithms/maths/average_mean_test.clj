(ns clojure-algorithms.maths.average-mean-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.average-mean :as sut]))

(deftest test-fn-mean-examples
  (is (approx 12.0 (sut/mean [3 6 9 12 15 18 21])))
  (is (approx 20.0 (sut/mean [5 10 15 20 25 30 35])))
  (is (approx 4.5 (sut/mean [1 2 3 4 5 6 7 8])))
  (is (thrown? Exception (sut/mean []))))
