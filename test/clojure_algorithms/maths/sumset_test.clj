(ns clojure-algorithms.maths.sumset-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sumset :as sut]))

(deftest test-fn-sumset-examples
  (is (approx #{5 6 7 8 9} (sut/sumset (set [1 2 3]) (set [4 5 6]))))
  (is (approx #{5 6 7 8 9 10} (sut/sumset (set [1 2 3]) (set [4 5 6 7])))))
