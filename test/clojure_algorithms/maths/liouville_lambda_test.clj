(ns clojure-algorithms.maths.liouville-lambda-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.liouville-lambda :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/liouville-lambda)))
