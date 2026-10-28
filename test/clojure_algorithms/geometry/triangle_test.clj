(ns clojure-algorithms.geometry.triangle-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.triangle
             :refer [make-point make-triangle circumcenter incenter orthocenter]]))

(defn- approx? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-triangle-centres
  (let [t (make-triangle (make-point -1.0 0.0) (make-point 1.0 0.0)
                         (make-point 0.0 1.0))
        cc (circumcenter t) ic (incenter t) oc (orthocenter t)]
    (is (approx? (:x cc) 0.0)) (is (approx? (:y cc) 0.0))
    (is (approx? (:x ic) 0.0)) (is (approx? (:y ic) 0.4142135623730951))
    (is (approx? (:x oc) 0.0)) (is (approx? (:y oc) 1.0))))

(deftest test-triangle-validation
  (is (thrown? IllegalArgumentException
               (make-triangle (make-point 0 0) (make-point 0 0)
                              (make-point 1 1))))
  (is (thrown? IllegalArgumentException
               (make-triangle (make-point 0 0) (make-point 1 1)
                              (make-point 2 2))))
  (is (thrown? IllegalArgumentException (make-point "a" 0))))
