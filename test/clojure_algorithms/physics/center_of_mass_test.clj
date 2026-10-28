(ns clojure-algorithms.physics.center-of-mass-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.center-of-mass
             :refer [center-of-mass make-particle]]))

(deftest test-center-of-mass
  (testing "known values"
    (is (= {:x 6.61 :y 7.98 :z 8.69}
           (center-of-mass [(make-particle 1.5 4 3.4 4)
                            (make-particle 5 6.8 7 8.1)
                            (make-particle 9.4 10.1 11.6 12)])))
    (is (= {:x 6.33 :y 7.33 :z 8.33}
           (center-of-mass [(make-particle 1 2 3 4)
                            (make-particle 5 6 7 8)
                            (make-particle 9 10 11 12)]))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (center-of-mass [(make-particle 1 2 3 -4)
                                  (make-particle 5 6 7 8)])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (center-of-mass [(make-particle 1 2 3 0)
                                  (make-particle 5 6 7 8)])))
    (is (thrown? clojure.lang.ExceptionInfo (center-of-mass [])))))
