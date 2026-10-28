(ns clojure-algorithms.physics.mirror-formulae-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.mirror-formulae
             :refer [focal-length image-distance object-distance]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-mirror-formulae
  (testing "focal length"
    (is (approx= (focal-length 10 20) 6.66666666666666 1e-9))
    (is (approx= (focal-length 9.5 6.7) 3.929012346 1e-9))
    (is (thrown? clojure.lang.ExceptionInfo (focal-length 0 20))))
  (testing "object distance"
    (is (approx= (object-distance 30 20) -60.0 1e-9))
    (is (approx= (object-distance 10.5 11.7) 102.375 1e-9))
    (is (thrown? clojure.lang.ExceptionInfo (object-distance 90 0))))
  (testing "image distance"
    (is (approx= (image-distance 10 40) 13.33333333 1e-6))
    (is (approx= (image-distance 1.5 6.7) 1.932692308 1e-9))
    (is (thrown? clojure.lang.ExceptionInfo (image-distance 0 0)))))
