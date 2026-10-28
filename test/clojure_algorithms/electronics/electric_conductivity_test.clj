(ns clojure-algorithms.electronics.electric-conductivity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.electric-conductivity :as ec]))

(deftest electric-conductivity-test
  (testing "solved quantities"
    (let [[n v] (ec/electric-conductivity 25 100 0)]
      (is (= "mobility" n))
      (is (< (Math/abs (- v 1.5604519068722301e+18)) 1e3)))
    (let [[n v] (ec/electric-conductivity 0 1600 200)]
      (is (= "conductivity" n))
      (is (< (Math/abs (- v 5.12672e-14)) 1e-28)))
    (let [[n v] (ec/electric-conductivity 1000 0 1200)]
      (is (= "electron_conc" n))
      (is (< (Math/abs (- v 5.201506356240767e+18)) 1e3))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ec/electric-conductivity -10 100 0)))
    (is (thrown? IllegalArgumentException (ec/electric-conductivity 50 -10 0)))
    (is (thrown? IllegalArgumentException (ec/electric-conductivity 50 0 -10)))
    (is (thrown? IllegalArgumentException (ec/electric-conductivity 50 0 0)))
    (is (thrown? IllegalArgumentException (ec/electric-conductivity 50 200 300)))))
