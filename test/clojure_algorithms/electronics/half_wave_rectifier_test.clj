(ns clojure-algorithms.electronics.half-wave-rectifier-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.half-wave-rectifier :as hw]))

(defn- approx [expected actual]
  (< (Math/abs (- expected actual)) 1e-9))

(deftest half-wave-rectifier-test
  (testing "rectifier quantities"
    (let [r (hw/max-load-current 2 4 6 15)]
      (is (= "max_load_current" (:name r)))
      (is (approx 1.25 (:value r))))
    (is (= 0.0 (:value (hw/max-load-current 2 4 6 0))))
    (is (approx 0.636 (:value (hw/dc-current 2))))
    (is (approx 0.0 (:value (hw/dc-current 0))))
    (is (approx 0.636 (:value (hw/dc-voltage 2))))
    (is (approx 0.0 (:value (hw/dc-voltage 0))))
    (let [r (hw/max-current 2 5)]
      (is (= "Max_current_Im" (:name r)))
      (is (approx 0.4 (:value r))))
    (is (= {:name "Irms" :value 5.0} (hw/rms-current 10)))
    (is (= {:name "Irms" :value 0.0} (hw/rms-current 0)))
    (is (= {:name "Vrms" :value 10.0} (hw/rms-voltage 20)))
    (is (= {:name "Vrms" :value 0.0} (hw/rms-voltage 0))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (hw/max-load-current 2 -4 6 15)))
    (is (thrown? IllegalArgumentException (hw/max-load-current 0 0 0 15)))
    (is (thrown? IllegalArgumentException (hw/max-current 2 -5)))))
