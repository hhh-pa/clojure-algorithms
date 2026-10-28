(ns clojure-algorithms.maths.disarium-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.disarium :as sut]))

(deftest test-fn-is-disarium-examples
  (is (= true (boolean (sut/is-disarium 89))))
  (is (= false (boolean (sut/is-disarium 75))))
  (is (= true (boolean (sut/is-disarium 135)))))
