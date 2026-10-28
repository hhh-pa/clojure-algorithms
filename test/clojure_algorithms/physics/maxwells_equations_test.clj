(ns clojure-algorithms.physics.maxwells-equations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.maxwells-equations
             :refer [ampere-maxwell-law electromagnetic-wave-impedance
                     electromagnetic-wave-speed energy-density-electromagnetic
                     faraday-law gauss-law-electric gauss-law-magnetic
                     poynting-vector-magnitude vacuum-permeability
                     vacuum-permittivity]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-gauss-laws
  (testing "electric"
    (is (true? (gauss-law-electric 1000 1.0 8.854e-9)))
    (is (true? (gauss-law-electric 500 2.0 8.854e-9)))
    (is (false? (gauss-law-electric -100 1.0 8.854e-9)))
    (is (thrown? clojure.lang.ExceptionInfo (gauss-law-electric 1 -1 1)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (gauss-law-electric 1 1 1 :permittivity 0))))
  (testing "magnetic"
    (is (true? (gauss-law-magnetic 2.0)))
    (is (true? (gauss-law-magnetic 0.0)))
    (is (true? (gauss-law-magnetic 5.0)))
    (is (thrown? clojure.lang.ExceptionInfo (gauss-law-magnetic -1.0)))))

(deftest test-faraday-and-ampere
  (testing "faraday"
    (is (true? (faraday-law 10.0 -10.0)))
    (is (true? (faraday-law -5.0 5.0)))
    (is (true? (faraday-law 0.0 0.0)))
    (is (false? (faraday-law 10.0 10.0))))
  (testing "ampere-maxwell"
    (is (true? (ampere-maxwell-law 1.256e-6 1.0 0.0)))
    (is (true? (ampere-maxwell-law 2.512e-6 2.0 0.0)))
    (is (true? (ampere-maxwell-law 1.11e-5 0.0 1.0e12)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ampere-maxwell-law 1 1 0 :permeability 0)))))

(deftest test-derived-quantities
  (testing "wave speed"
    (is (approx= (electromagnetic-wave-speed) 2.998e8 1e6))
    (is (approx= (electromagnetic-wave-speed
                  :permeability vacuum-permeability
                  :permittivity (* 2 vacuum-permittivity))
                 2.12e8 1e7)))
  (testing "wave impedance"
    (is (approx= (electromagnetic-wave-impedance) 376.73 0.01))
    (is (approx= (electromagnetic-wave-impedance
                  :permeability (* 2 vacuum-permeability)
                  :permittivity vacuum-permittivity)
                 532.0 1.0)))
  (testing "poynting vector"
    (is (approx= (poynting-vector-magnitude 1000 1e-6) 795.8 1.0))
    (is (approx= (poynting-vector-magnitude 377 1.0) 3.0e8 1e6))
    (is (approx= (poynting-vector-magnitude 0 1.0) 0.0 1e-9)))
  (testing "energy density"
    (is (approx= (energy-density-electromagnetic 1000 1e-3) 0.398 0.001))
    (is (approx= (energy-density-electromagnetic 0 1.0) 397887 1.0))
    (is (approx= (energy-density-electromagnetic 377 1e-6) 1.0e-6 1e-6)))
  (testing "invalid constants throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (electromagnetic-wave-speed :permeability -1)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (electromagnetic-wave-impedance :permittivity 0)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (poynting-vector-magnitude 1 1 :permeability 0)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (energy-density-electromagnetic 1 1 :permittivity -1)))))
