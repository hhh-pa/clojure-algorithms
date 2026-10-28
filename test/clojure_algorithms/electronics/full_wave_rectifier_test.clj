(ns clojure-algorithms.electronics.full-wave-rectifier-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.full-wave-rectifier :as fw]))

(defn- approx [expected actual]
  (< (Math/abs (- expected actual)) 1e-9))

(deftest full-wave-rectifier-test
  (testing "rectifier quantities"
    (let [r (fw/max-load-current 2 4 6 15)]
      (is (= "max_load_current" (:name r)))
      (is (approx 1.25 (:value r))))
    (is (= 0.0 (:value (fw/max-load-current 2 4 6 0))))
    (let [r (fw/dc-current 2)]
      (is (= "Idc" (:name r)))
      (is (approx 1.272 (:value r))))
    (is (approx 0.0 (:value (fw/dc-current 0))))
    (is (approx 1.272 (:value (fw/dc-voltage 2))))
    (is (approx 0.0 (:value (fw/dc-voltage 0))))
    (let [r (fw/max-current 2 5)]
      (is (= "Max_current_Im" (:name r)))
      (is (approx 0.4 (:value r))))
    (is (approx 7.07 (:value (fw/rms-current 10))))
    (is (approx 0.0 (:value (fw/rms-current 0))))
    (is (approx 14.14 (:value (fw/rms-voltage 20))))
    (is (approx 0.0 (:value (fw/rms-voltage 0)))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (fw/max-load-current 2 -4 6 15)))
    (is (thrown? IllegalArgumentException (fw/max-load-current 0 0 0 15)))
    (is (thrown? IllegalArgumentException (fw/max-current 2 -5)))))
