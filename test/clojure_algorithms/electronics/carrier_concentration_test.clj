(ns clojure-algorithms.electronics.carrier-concentration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.carrier-concentration :as cc]))

(deftest carrier-concentration-test
  (testing "solved concentrations"
    (is (= ["intrinsic_conc" 50.0] (cc/carrier-concentration 25 100 0)))
    (is (= ["electron_conc" 25.0] (cc/carrier-concentration 0 1600 200)))
    (is (= ["hole_conc" 1440.0] (cc/carrier-concentration 1000 0 1200))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (cc/carrier-concentration 1000 400 1200)))
    (is (thrown? IllegalArgumentException (cc/carrier-concentration -1000 0 1200)))
    (is (thrown? IllegalArgumentException (cc/carrier-concentration 0 -400 1200)))
    (is (thrown? IllegalArgumentException (cc/carrier-concentration 0 400 -1200)))))
