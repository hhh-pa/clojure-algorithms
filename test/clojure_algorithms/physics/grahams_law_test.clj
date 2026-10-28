(ns clojure-algorithms.physics.grahams-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.grahams-law
             :refer [effusion-ratio first-effusion-rate first-molar-mass
                     second-effusion-rate second-molar-mass valid?]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-valid?
  (is (true? (valid? 2.016 4.002)))
  (is (false? (valid? -2.016 4.002)))
  (is (false? (valid?))))

(deftest test-grahams-law
  (testing "known values"
    (is (approx= (effusion-ratio 2.016 4.002) 1.408943 1e-6))
    (is (approx= (first-effusion-rate 1 2.016 4.002) 1.408943 1e-6))
    (is (approx= (second-effusion-rate 1 2.016 4.002) 0.709752 1e-6))
    (is (approx= (first-molar-mass 2 1.408943 0.709752) 0.507524 1e-6))
    (is (approx= (second-molar-mass 2 1.408943 0.709752) 1.970351 1e-6)))
  (testing "non-positive inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (effusion-ratio -2.016 4.002)))
    (is (thrown? clojure.lang.ExceptionInfo (first-effusion-rate -1 2.016 4.002)))
    (is (thrown? clojure.lang.ExceptionInfo (second-effusion-rate -1 2.016 4.002)))
    (is (thrown? clojure.lang.ExceptionInfo (first-molar-mass -1 2.016 4.002)))
    (is (thrown? clojure.lang.ExceptionInfo (second-molar-mass -2 1.408943 0.709752)))))
