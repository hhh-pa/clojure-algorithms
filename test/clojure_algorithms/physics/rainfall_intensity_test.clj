(ns clojure-algorithms.physics.rainfall-intensity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.rainfall-intensity :refer [rainfall-intensity]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-rainfall-intensity
  (testing "known values"
    (is (approx= (rainfall-intensity 1000 0.2 11.6 0.81 10 60) 49.83339231138578 1e-9))
    (is (approx= (rainfall-intensity 1000 0.2 11.6 0.81 10 30) 77.36319588106228 1e-9))
    (is (approx= (rainfall-intensity 1000 0.2 11.6 0.81 5 60) 43.382487747633625 1e-9)))
  (testing "non-positive parameters throw"
    (doseq [args [[0 0.2 11.6 0.81 10 60] [1000 -0.2 11.6 0.81 10 60]
                  [1000 0.2 -11.6 0.81 10 60] [1000 0.2 11.6 -0.81 10 60]
                  [1000 0.2 11.6 0.81 0 60] [1000 0.2 11.6 0.81 10 0]]]
      (is (thrown? clojure.lang.ExceptionInfo (apply rainfall-intensity args))))))
