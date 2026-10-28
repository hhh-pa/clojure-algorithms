(ns clojure-algorithms.maths.euclidean-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.euclidean-distance :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/euclidean-distance))
  (is (some? #'sut/euclidean-distance-no-np))
  (is (some? #'sut/Vector))
  (is (some? #'sut/VectorOut)))
