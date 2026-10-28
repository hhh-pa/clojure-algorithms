(ns clojure-algorithms.electronics.star-delta-conversions-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.star-delta-conversions :as sd]))

(defn- approx-z [expected actual]
  (and (= (count expected) (count actual))
       (every? (fn [[[er ei] [ar ai]]]
                 (and (< (Math/abs (- er ar)) 1e-9)
                      (< (Math/abs (- ei ai)) 1e-9)))
               (map vector expected actual))))

(deftest star-to-delta-test
  (testing "star to delta"
    (is (approx-z [[2.5 0.0] [5.0 0.0] [5.0 0.0]]
                  (sd/star-to-delta [2 0] [1 0] [1 0])))
    (is (approx-z [[-3.5 12.0] [8.2 7.6] [3.702702702702702 1.7837837837837838]]
                  (sd/star-to-delta [2 0] [1 2] [1 6]))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (sd/star-to-delta [0 0] [1 2] [1 6])))
    (is (thrown? IllegalArgumentException
                 (sd/star-to-delta [12 0] [-1 2] [1 6])))))

(deftest delta-to-star-test
  (testing "delta to star"
    (is (approx-z [[2.3333333333333335 0.0] [1.4 0.0] [1.0 0.0]]
                  (sd/delta-to-star [3 0] [5 0] [7 0])))
    (is (approx-z [[0.0 -2.0] [3.0 -1.0] [0.6666666666666666 2.0]]
                  (sd/delta-to-star [1 3] [2 0] [0 -3]))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (sd/delta-to-star [0 0] [1 2] [1 6])))
    (is (thrown? IllegalArgumentException
                 (sd/delta-to-star [12 0] [-1 2] [1 6])))))
