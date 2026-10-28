(ns clojure-algorithms.maths.weddles-rule-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.weddles-rule :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/get-inputs))
  (is (some? #'sut/safe-function-eval))
  (is (some? #'sut/compute-table))
  (is (some? #'sut/apply-weights))
  (is (some? #'sut/compute-solution)))
