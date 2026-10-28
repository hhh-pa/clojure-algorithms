(ns clojure-algorithms.physics.newtons-second-law-of-motion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.newtons-second-law-of-motion
             :refer [newtons-second-law-of-motion]]))

(deftest test-newtons-second-law-of-motion
  (testing "known values"
    (is (= 100 (newtons-second-law-of-motion 10 10)))
    (is (= 2.0 (newtons-second-law-of-motion 2.0 1)))
    (is (= 125.0 (newtons-second-law-of-motion 12.5 10))))
  (testing "invalid inputs yield -0.0"
    (is (= -0.0 (newtons-second-law-of-motion "a" 10)))))
