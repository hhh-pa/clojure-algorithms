(ns clojure-algorithms.matrix.sherman-morrison-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.sherman-morrison :as sut]))

(deftest test-fn-init-examples
  (is (some? (let [a (sut/make-Matrix 2 3 1)] a))))
