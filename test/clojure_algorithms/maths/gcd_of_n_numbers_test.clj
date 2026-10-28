(ns clojure-algorithms.maths.gcd-of-n-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.gcd-of-n-numbers :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/get-factors))
  (is (some? #'sut/get-greatest-common-divisor)))
