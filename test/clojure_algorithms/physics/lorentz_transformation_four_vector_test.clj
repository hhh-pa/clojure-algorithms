(ns clojure-algorithms.physics.lorentz-transformation-four-vector-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.lorentz-transformation-four-vector
             :refer [beta c lorentz-gamma transform transformation-matrix]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-beta
  (testing "known values"
    (is (approx= (beta c) 1.0 1e-12))
    (is (approx= (beta 199792458) 0.666435904801848 1e-12))
    (is (approx= (beta 1e5) 0.00033356409519815205 1e-15)))
  (testing "invalid speeds throw"
    (is (thrown? clojure.lang.ExceptionInfo (beta 0.2)))
    (is (thrown? clojure.lang.ExceptionInfo (beta (* 2 c))))))

(deftest test-lorentz-gamma
  (testing "known values"
    (is (approx= (lorentz-gamma 4) 1.0000000000000002 1e-12))
    (is (approx= (lorentz-gamma 1e5) 1.0000000556325075 1e-12))
    (is (approx= (lorentz-gamma 3e7) 1.005044845777813 1e-12))
    (is (approx= (lorentz-gamma 2.8e8) 2.7985595722318277 1e-12))
    (is (approx= (lorentz-gamma 299792451) 4627.49902669495 1e-6)))
  (testing "invalid speeds throw"
    (is (thrown? clojure.lang.ExceptionInfo (lorentz-gamma 0.3)))
    (is (thrown? clojure.lang.ExceptionInfo (lorentz-gamma (* 2 c))))))

(deftest test-transformation-matrix
  (testing "known entries"
    (let [m (transformation-matrix 29979245)]
      (is (approx= (get-in m [0 0]) 1.00503781 1e-6))
      (is (approx= (get-in m [0 1]) -0.10050378 1e-6))
      (is (approx= (get-in m [2 2]) 1.0 1e-12))))
  (testing "invalid speeds throw"
    (is (thrown? clojure.lang.ExceptionInfo (transformation-matrix 0)))
    (is (thrown? clojure.lang.ExceptionInfo (transformation-matrix (* c 1.5))))))

(deftest test-transform
  (testing "numeric event"
    (let [[ct x y z] (transform 29979245 [1 2 3 4])]
      (is (approx= ct 3.01302757e+08 1e3))
      (is (approx= x -3.01302729e+07 1e3))
      (is (approx= y 3.0 1e-9))
      (is (approx= z 4.0 1e-9))))
  (testing "invalid speeds throw"
    (is (thrown? clojure.lang.ExceptionInfo (transform 299792459 [1 1 1 1])))
    (is (thrown? clojure.lang.ExceptionInfo (transform -1 [1 1 1 1])))))
