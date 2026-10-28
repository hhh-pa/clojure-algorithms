(ns clojure-algorithms.maths.radians-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.radians :as sut]))

(deftest test-fn-radians-examples
  (is (approx 3.141592653589793 (sut/radians 180)))
  (is (approx 1.6057029118347832 (sut/radians 92)))
  (is (approx 4.782202150464463 (sut/radians 274)))
  (is (approx 1.9167205845401725 (sut/radians 109.82))))

(deftest test-fn-radians-extra
  (is (every? (fn [i]
                (<= (Math/abs (- (sut/radians i) (Math/toRadians i))) 1e-8))
              (range -2 361))))
