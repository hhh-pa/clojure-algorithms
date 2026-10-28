(ns clojure-algorithms.electronics.electrical-impedance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.electrical-impedance :as ei]))

(deftest electrical-impedance-test
  (testing "solved quantities"
    (is (= {"impedance" 5.0} (ei/electrical-impedance 3 4 0)))
    (is (= {"resistance" 3.0} (ei/electrical-impedance 0 4 5)))
    (is (= {"reactance" 4.0} (ei/electrical-impedance 3 0 5))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ei/electrical-impedance 3 4 5)))
    (is (thrown? IllegalArgumentException (ei/electrical-impedance 0 0 5)))))
