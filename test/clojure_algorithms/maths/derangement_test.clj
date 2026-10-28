(ns clojure-algorithms.maths.derangement-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.derangement :as sut]))

(deftest test-fn-derangement-examples
  (is (approx 2 (sut/derangement 3)))
  (is (approx 44 (sut/derangement 5)))
  (is (approx 1334961 (sut/derangement 10))))
