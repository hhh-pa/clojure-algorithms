(ns clojure-algorithms.maths.prime-check-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.prime-check :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/is-prime)))
