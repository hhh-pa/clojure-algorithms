(ns clojure-algorithms.maths.numerical-analysis.adams-bashforth-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.adams-bashforth :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-AdamsBashforth))
  (is (some? #'sut/AdamsBashforth-post-init))
  (is (some? #'sut/AdamsBashforth-step-2))
  (is (some? #'sut/AdamsBashforth-step-3))
  (is (some? #'sut/AdamsBashforth-step-4))
  (is (some? #'sut/AdamsBashforth-step-5)))
