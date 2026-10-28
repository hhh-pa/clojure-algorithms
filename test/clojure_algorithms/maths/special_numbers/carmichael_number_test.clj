(ns clojure-algorithms.maths.special-numbers.carmichael-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.carmichael-number :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/power))
  (is (some? #'sut/is-carmichael-number)))
