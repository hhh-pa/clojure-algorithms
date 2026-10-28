(ns clojure-algorithms.maths.sum-of-geometric-progression-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.sum-of-geometric-progression :as sut]))

(deftest test-fn-sum-of-geometric-progression-examples
  (is (approx 1023.0 (sut/sum-of-geometric-progression 1 2 10)))
  (is (approx 11111.0 (sut/sum-of-geometric-progression 1 10 5)))
  (is (approx 0.0 (sut/sum-of-geometric-progression 0 2 10)))
  (is (approx 1.0 (sut/sum-of-geometric-progression 1 0 10)))
  (is (approx -0.0 (sut/sum-of-geometric-progression 1 2 0)))
  (is (approx -1023.0 (sut/sum-of-geometric-progression -1 2 10)))
  (is (approx -341.0 (sut/sum-of-geometric-progression 1 -2 10)))
  (is (approx -0.9990234375 (sut/sum-of-geometric-progression 1 2 -10)))
  (is (approx 5 (sut/sum-of-geometric-progression 1 1 5))))
