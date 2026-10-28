(ns clojure-algorithms.physics.speed-of-sound-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.speed-of-sound :refer [speed-of-sound-in-a-fluid]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-speed-of-sound-in-a-fluid
  (testing "known values"
    (is (approx= (speed-of-sound-in-a-fluid 998 2.15e9) 1467.7563207952705 1e-9))
    (is (approx= (speed-of-sound-in-a-fluid 13600 28.5e9) 1447.614670861731 1e-9)))
  (testing "non-positive inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (speed-of-sound-in-a-fluid 0 2.15e9)))
    (is (thrown? clojure.lang.ExceptionInfo (speed-of-sound-in-a-fluid 998 -1)))))
