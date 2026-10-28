(ns clojure-algorithms.maths.special-numbers.spy-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.spy-number :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/is-spy-number)))
