(ns clojure-algorithms.other.scoring-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.scoring-algorithm :as s]))

(deftest scoring-test
  (testing "get-data transposes"
    (is (= [[20.0 23.0 22.0] [60.0 90.0 50.0] [2012.0 2015.0 2011.0]]
           (s/get-data [[20 60 2012] [23 90 2015] [22 50 2011]]))))
  (testing "calculate-each-score"
    (let [scores (s/calculate-each-score [[20 23 22] [60 90 50] [2012 2015 2011]]
                                         [0 0 1])]
      (is (= 3 (count scores)))
      (is (< (abs (- (get-in scores [0 2]) 0.33333333333333337)) 1e-9))
      (is (= [1.0 0.0] [(get-in scores [0 0]) (get-in scores [0 1])]))))
  (testing "generate-final-scores"
    (is (= [2.0 1.0 1.3333333333333335]
           (s/generate-final-scores [[1.0 0.0 0.33333333333333337]
                                     [0.75 0.0 1.0]
                                     [0.25 1.0 0.0]]))))
  (testing "procentual-proximity appends totals"
    (is (= [[20 60 2012 2.0] [23 90 2015 1.0] [22 50 2011 1.3333333333333335]]
           (s/procentual-proximity [[20 60 2012] [23 90 2015] [22 50 2011]]
                                   [0 0 1]))))
  (testing "invalid weight throws"
    (is (thrown? IllegalArgumentException
                 (s/calculate-each-score [[1 2]] [2])))))
