(ns clojure-algorithms.physics.casimir-effect-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.casimir-effect :refer [casimir-force]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-casimir-force
  (testing "known values"
    (is (approx= (:force (casimir-force 0 4 0.03)) 6.4248189174864216e-21 1e-30))
    (is (approx= (:distance (casimir-force 2635e-13 0.0023 0)) 1.0323056015031114e-05 1e-14))
    (is (approx= (:area (casimir-force 2737e-21 0 0.0023746)) 0.06688838837354052 1e-12)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (casimir-force 3457e-12 0 0)))
    (is (thrown? clojure.lang.ExceptionInfo (casimir-force 3457e-12 0 -0.00344)))
    (is (thrown? clojure.lang.ExceptionInfo (casimir-force -912e-12 0 0.09374)))))
