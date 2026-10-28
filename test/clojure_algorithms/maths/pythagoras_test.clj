(ns clojure-algorithms.maths.pythagoras-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.pythagoras :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/distance))
  (is (some? #'sut/make-Point))
  (is (some? #'sut/Point-repr)))
