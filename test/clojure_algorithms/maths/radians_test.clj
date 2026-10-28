(ns clojure-algorithms.maths.radians-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.radians :as sut]))

(deftest test-fn-radians-examples
  (is (approx 3.141592653589793 (sut/radians 180)))
  (is (approx 1.6057029118347832 (sut/radians 92)))
  (is (approx 4.782202150464463 (sut/radians 274)))
  (is (approx 1.9167205845401725 (sut/radians 109.82))))
