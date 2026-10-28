(ns clojure-algorithms.matrix.max-area-of-island-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.max-area-of-island :as sut]))

(deftest test-fn-is-safe-examples
  (is (= true (boolean (sut/is-safe 0 0 5 5))))
  (is (= false (boolean (sut/is-safe -1 -1 5 5)))))
