(ns clojure-algorithms.project-euler.problem-587.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-587.sol1 :refer [circle-bottom-arc-integral
                                                                       concave-triangle-area
                                                                       solution]]))

(defn- approx=
  "Returns true when the numbers differ by less than tol."
  [expected actual tol]
  (< (Math/abs (- (double expected) (double actual))) tol))

(deftest circle-bottom-arc-integral-test
  (is (approx= 0.39269908169872414 (circle-bottom-arc-integral 0) 1e-12))
  (is (approx= 0.44634954084936207 (circle-bottom-arc-integral (/ 1 2)) 1e-12))
  (is (approx= 0.5 (circle-bottom-arc-integral 1) 1e-12)))

(deftest concave-triangle-area-test
  (is (approx= 0.026825229575318944 (concave-triangle-area 1) 1e-12))
  (is (approx= 0.01956236140083944 (concave-triangle-area 2) 1e-12)))

(deftest solution-test
  (is (= 15 (solution (/ 1 10)))))
