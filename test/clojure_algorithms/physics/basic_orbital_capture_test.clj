(ns clojure-algorithms.physics.basic-orbital-capture-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.basic-orbital-capture
             :refer [capture-area capture-radii c]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-capture-radii
  (testing "known value"
    (is (approx= (capture-radii 6.957e8 1.99e30 25000.0) 17209590691.0 1.0)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (capture-radii -6.957e8 1.99e30 25000.0)))
    (is (thrown? clojure.lang.ExceptionInfo (capture-radii 6.957e8 -1.99e30 25000.0)))
    (is (thrown? clojure.lang.ExceptionInfo (capture-radii 6.957e8 1.99e30 (inc c))))))

(deftest test-capture-area
  (testing "known value"
    (is (approx= (capture-area 17209590691) 9.304455331329126e+20 1e11)))
  (testing "negative radius throws"
    (is (thrown? clojure.lang.ExceptionInfo (capture-area -1)))))
