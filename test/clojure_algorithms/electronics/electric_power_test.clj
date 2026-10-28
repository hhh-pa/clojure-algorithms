(ns clojure-algorithms.electronics.electric-power-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.electric-power :as ep]))

(deftest electric-power-test
  (testing "solved quantities"
    (is (= {:name "voltage" :value 2.5} (ep/electric-power 0 2 5)))
    (is (= {:name "power" :value 4.0} (ep/electric-power 2 2 0)))
    (is (= {:name "power" :value 6.0} (ep/electric-power -2 3 0)))
    (is (= {:name "power" :value 4.84} (ep/electric-power 2.2 2.2 0)))
    (is (= {:name "current" :value 3.0} (ep/electric-power 2 0 6))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ep/electric-power 2 4 2)))
    (is (thrown? IllegalArgumentException (ep/electric-power 0 0 2)))
    (is (thrown? IllegalArgumentException (ep/electric-power 0 2 -4)))))
