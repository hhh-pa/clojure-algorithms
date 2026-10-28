(ns clojure-algorithms.electronics.coulombs-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.coulombs-law :as cl]))

(deftest coulombs-law-test
  (testing "solved quantities"
    (let [f (get (cl/coulombs-law 0 3 5 2000) "force")]
      (is (< (Math/abs (- f 33705.0)) 1e-9)))
    (let [d (get (cl/coulombs-law 10 3 5 0) "distance")]
      (is (< (Math/abs (- d 116112.01488218177)) 1e-6)))
    (let [c (get (cl/coulombs-law 10 0 5 2000) "charge1")]
      (is (< (Math/abs (- c 0.0008900756564307966)) 1e-18))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (cl/coulombs-law 0 0 5 2000)))
    (is (thrown? IllegalArgumentException (cl/coulombs-law 0 3 5 -2000)))))
