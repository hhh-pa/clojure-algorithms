(ns clojure-algorithms.maths.floor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.floor :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/floor)))
