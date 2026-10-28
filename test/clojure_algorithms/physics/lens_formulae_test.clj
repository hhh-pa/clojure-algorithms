(ns clojure-algorithms.physics.lens-formulae-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.lens-formulae
             :refer [focal-length-of-lens image-distance object-distance]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-lens-formulae
  (testing "focal length"
    (is (approx= (focal-length-of-lens 10 4) 6.666666666666667 1e-9))
    (is (approx= (focal-length-of-lens 2.7 5.8) -5.0516129032258075 1e-9))
    (is (thrown? clojure.lang.ExceptionInfo (focal-length-of-lens 0 20))))
  (testing "object distance"
    (is (approx= (object-distance 10 40) -13.333333333333332 1e-9))
    (is (approx= (object-distance 6.2 1.5) 1.9787234042553192 1e-9))
    (is (thrown? clojure.lang.ExceptionInfo (object-distance 0 20))))
  (testing "image distance"
    (is (approx= (image-distance 50 40) 22.22222222222222 1e-9))
    (is (approx= (image-distance 5.3 7.9) 3.1719696969696973 1e-9))
    (is (thrown? clojure.lang.ExceptionInfo (image-distance 0 20)))))
