(ns clojure-algorithms.physics.first-law-of-thermodynamics-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.first-law-of-thermodynamics
             :refer [categorize-system heat-added internal-energy-variation
                     work-done]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-categorize-system
  (testing "categorization messages"
    (is (= "The system is isochoric (constant volume)."
           (categorize-system 0 :work)))
    (is (= "The system is endothermic (absorbing heat)."
           (categorize-system 50 :heat)))
    (is (= "The internal energy of the system is decreasing. It cooling down."
           (categorize-system -20 :internal-energy-variation))))
  (testing "invalid name throws"
    (is (thrown? clojure.lang.ExceptionInfo (categorize-system 10 :invalid)))))

(deftest test-first-law
  (testing "work"
    (is (approx= (work-done 50.0 -20.0) 70.0 1e-9))
    (is (approx= (work-done 50.0 50.0) 0.0 1e-9))
    (is (approx= (work-done -50.0 20.0) -70.0 1e-9)))
  (testing "heat"
    (is (approx= (heat-added -20.0 30.0) 10.0 1e-9))
    (is (approx= (heat-added 50.0 0.0) 50.0 1e-9))
    (is (approx= (heat-added 20.0 -70.0) -50.0 1e-9)))
  (testing "internal energy variation"
    (is (approx= (internal-energy-variation 50.0 30.0) 20.0 1e-9))
    (is (approx= (internal-energy-variation 50.0 0.0) 50.0 1e-9))
    (is (approx= (internal-energy-variation -50.0 -70.0) 20.0 1e-9)))
  (testing "non-numeric input throws"
    (is (thrown? clojure.lang.ExceptionInfo (work-done "50" 1.0)))))
