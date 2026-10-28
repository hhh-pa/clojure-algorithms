(ns clojure-algorithms.physics.archimedes-principle-of-buoyant-force-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.archimedes-principle-of-buoyant-force
             :refer [archimedes-principle]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-archimedes-principle
  (testing "known values"
    (is (approx= (archimedes-principle 500 4 9.8) 19600.0 1e-6))
    (is (approx= (archimedes-principle 997 0.5 9.8) 4885.3 1e-6))
    (is (approx= (archimedes-principle 997 0.7) 6844.061035 1e-6))
    (is (approx= (archimedes-principle 997 0.7 0) 0.0 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (archimedes-principle 997 -0.7)))
    (is (thrown? clojure.lang.ExceptionInfo (archimedes-principle 0 0.7)))
    (is (thrown? clojure.lang.ExceptionInfo (archimedes-principle 997 0.7 -9.8)))))
