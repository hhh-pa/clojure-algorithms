(ns clojure-algorithms.maths.sigmoid-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.sigmoid :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/sigmoid)))
