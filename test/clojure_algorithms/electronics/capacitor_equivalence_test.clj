(ns clojure-algorithms.electronics.capacitor-equivalence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.capacitor-equivalence :as ce]))

(deftest parallel-test
  (testing "parallel capacitance"
    (is (< (Math/abs (- (ce/capacitor-parallel [5.71389 12 3]) 20.71389)) 1e-9))
    (is (thrown? IllegalArgumentException
                 (ce/capacitor-parallel [5.71389 12 -3])))))

(deftest series-test
  (testing "series capacitance"
    (is (< (Math/abs (- (ce/capacitor-series [5.71389 12 3]) 1.6901062252507735))
           1e-12))
    (is (thrown? IllegalArgumentException
                 (ce/capacitor-series [5.71389 12 -3])))
    (is (thrown? IllegalArgumentException
                 (ce/capacitor-series [5.71389 12 0.0])))))
