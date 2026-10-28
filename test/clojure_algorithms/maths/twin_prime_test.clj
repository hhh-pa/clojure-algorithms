(ns clojure-algorithms.maths.twin-prime-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.twin-prime :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/twin-prime)))
