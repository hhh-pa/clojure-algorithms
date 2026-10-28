(ns clojure-algorithms.electronics.power-factor-correction-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.power-factor-correction :as pf]))

(defn- approx [expected actual eps]
  (< (Math/abs (- expected actual)) eps))

(deftest capacitor-test
  (testing "shunt capacitance"
    (is (approx 0.00031043753362948597 (pf/shunt-capacitor-power-factor-correction
                                        120 60 4000 0.8 0.95) 1e-18))
    (is (approx 0.00021690547192207782 (pf/shunt-capacitor-power-factor-correction
                                        150 50 2000 0.6 0.87) 1e-18)))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (pf/shunt-capacitor-power-factor-correction 115 0 800 0.6 0.87)))
    (is (thrown? IllegalArgumentException
                 (pf/shunt-capacitor-power-factor-correction 0 60 800 0.6 0.87)))))

(deftest inductor-test
  (testing "shunt inductance"
    (is (approx 0.02266540783980564 (pf/shunt-inductor-power-factor-correction
                                     120 60 4000 0.8 0.95) 1e-15))
    (is (approx 0.006195660726930193 (pf/shunt-inductor-power-factor-correction
                                      120 60 4000 -0.8 -0.4) 1e-15)))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (pf/shunt-inductor-power-factor-correction 115 0 800 -0.6 0.87)))
    (is (thrown? IllegalArgumentException
                 (pf/shunt-inductor-power-factor-correction 0 60 800 -0.6 0.87)))
    (is (thrown? IllegalArgumentException
                 (pf/shunt-inductor-power-factor-correction 120 60 4000 0.8 0.8)))))
