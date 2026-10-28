(ns clojure-algorithms.electronics.charging-inductor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.charging-inductor :as ci]))

(defn- approx [expected actual]
  (< (Math/abs (- expected actual)) 1e-9))

(deftest charging-inductor-test
  (testing "charging currents"
    (is (approx 2.817 (ci/charging-inductor 5.8 1.5 2.3 2)))
    (is (approx 1.543 (ci/charging-inductor 8 5 3 2)))
    (is (approx 0.016 (ci/charging-inductor 8 (* 5 100) 3 2))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ci/charging-inductor -8 100 15 12)))
    (is (thrown? IllegalArgumentException (ci/charging-inductor 80 -15 100 5)))
    (is (thrown? IllegalArgumentException (ci/charging-inductor 12 200 -20 5)))
    (is (thrown? IllegalArgumentException (ci/charging-inductor 0 200 20 5)))
    (is (thrown? IllegalArgumentException (ci/charging-inductor 10 0 20 5)))
    (is (thrown? IllegalArgumentException (ci/charging-inductor 15 25 0 5)))))
