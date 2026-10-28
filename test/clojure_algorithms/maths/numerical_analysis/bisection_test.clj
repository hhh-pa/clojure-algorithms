(ns clojure-algorithms.maths.numerical-analysis.bisection-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.bisection :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/bisection))
  (is (some? #'sut/f)))
