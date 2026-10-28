(ns clojure-algorithms.geometry.braik-mac-construction-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure-algorithms.geometry.braik-mac-construction
             :refer [make-point conic-coefficients conic-generate determinant]]))

(deftest test-braik-mac-construction
  (is (= 1.0 (determinant [[1 0] [0 1]])))
  (is (< (Math/abs (- (determinant [[1 2] [3 4]]) -2.0)) 1e-9))
  (let [pts [(make-point 0.0 0.0) (make-point 5.0 0.0) (make-point 2.0 3.0)
             (make-point 1.0 10.0) (make-point 6.0 7.0)]
        [a b c d e f] (conic-coefficients pts)]
    (doseq [{:keys [x y]} pts]
      (is (< (Math/abs (+ (* a x x) (* b x y) (* c y y) (* d x) (* e y) f))
             1e-6)))
    (is (str/starts-with? (conic-generate pts) "0 = "))
    (is (str/includes? (conic-generate pts) "X**2")))
  (is (thrown? IllegalArgumentException
               (conic-coefficients [(make-point 0 0)]))))
