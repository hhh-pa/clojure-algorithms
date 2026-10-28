(ns clojure-algorithms.maths.euclidean-distance-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.euclidean-distance :as sut]))

(deftest test-fn-euclidean-distance-examples
  (is (approx 2.8284271247461903 (sut/euclidean-distance [0 0] [2 2])))
  (is (approx 3.4641016151377544 (sut/euclidean-distance [0 0 0] [2 2 2])))
  (is (approx 8.0 (sut/euclidean-distance [1 2 3 4] [5 6 7 8]))))

(deftest test-fn-euclidean-distance-no-np-examples
  (is (approx 2.8284271247461903 (sut/euclidean-distance-no-np [0 0] [2 2])))
  (is (approx 8.0 (sut/euclidean-distance-no-np [1 2 3 4] [5 6 7 8]))))

(deftest test-euclidean-distance-extra
  (is (approx 5.0 (sut/euclidean-distance [0.0 0.0] [3.0 4.0])))
  (is (approx 0.0 (sut/euclidean-distance [1 2 3] [1 2 3])))
  (is (approx (sut/euclidean-distance [1 2 3] [4 5 6])
              (sut/euclidean-distance-no-np [1 2 3] [4 5 6]))))
