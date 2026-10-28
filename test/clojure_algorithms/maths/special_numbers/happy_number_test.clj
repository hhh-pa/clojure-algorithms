(ns clojure-algorithms.maths.special-numbers.happy-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.happy-number :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/is-happy-number)))
