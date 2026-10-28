(ns clojure-algorithms.electronics.real-and-reactive-power-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.real-and-reactive-power :as rr]))

(deftest real-power-test
  (testing "real power"
    (is (= 90.0 (rr/real-power 100 0.9)))
    (is (= 0.0 (rr/real-power 0 0.8)))
    (is (= -90.0 (rr/real-power 100 -0.9)))
    (is (thrown? IllegalArgumentException (rr/real-power 100 1.5)))))

(deftest reactive-power-test
  (testing "reactive power"
    (is (< (Math/abs (- (rr/reactive-power 100 0.9) 43.58898943540673)) 1e-12))
    (is (= 0.0 (rr/reactive-power 0 0.8)))
    (is (< (Math/abs (- (rr/reactive-power 100 -0.9) 43.58898943540673)) 1e-12))
    (is (thrown? IllegalArgumentException (rr/reactive-power 100 2)))))
