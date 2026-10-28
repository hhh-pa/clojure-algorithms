(ns clojure-algorithms.maths.lucas-lehmer-primality-test-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.lucas-lehmer-primality-test :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/lucas-lehmer-test)))
