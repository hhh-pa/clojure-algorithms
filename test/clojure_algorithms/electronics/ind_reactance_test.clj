(ns clojure-algorithms.electronics.ind-reactance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.ind-reactance :as ir]))

(defn- approx [expected actual]
  (< (Math/abs (- expected actual)) 1e-12))

(deftest ind-reactance-test
  (testing "solved quantities"
    (is (approx 0.0007957747154594767
                (get (ir/ind-reactance 0 10e3 50) "inductance")))
    (is (approx 227.36420441699332
                (get (ir/ind-reactance 35e-3 0 50) "frequency")))
    (is (approx 0.2199114857512855
                (get (ir/ind-reactance 35e-6 1e3 0) "reactance"))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ir/ind-reactance -35e-6 1e3 0)))
    (is (thrown? IllegalArgumentException (ir/ind-reactance 35e-6 -1e3 0)))
    (is (thrown? IllegalArgumentException (ir/ind-reactance 35e-6 0 -1)))))
