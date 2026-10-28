(ns clojure-algorithms.maths.collatz-sequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.collatz-sequence :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/collatz-sequence))
  (is (some? #'sut/main)))
