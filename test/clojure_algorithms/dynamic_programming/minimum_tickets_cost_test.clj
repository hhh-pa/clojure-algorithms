(ns clojure-algorithms.dynamic-programming.minimum-tickets-cost-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-tickets-cost :as mt]))

(deftest mincost-tickets-test
  (testing "minimum ticket costs"
    (is (= 11 (mt/mincost-tickets [1 4 6 7 8 20] [2 7 15])))
    (is (= 17 (mt/mincost-tickets [1 2 3 4 5 6 7 8 9 10 30 31] [2 7 15])))
    (is (= 24 (mt/mincost-tickets [1 2 3 4 5 6 7 8 9 10 30 31] [2 90 150])))
    (is (= 2 (mt/mincost-tickets [2] [2 90 150])))
    (is (= 0 (mt/mincost-tickets [] [2 90 150]))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (mt/mincost-tickets "hello" [2 90 150])))
    (is (thrown? IllegalArgumentException (mt/mincost-tickets [] "world")))
    (is (thrown? IllegalArgumentException
                 (mt/mincost-tickets [0.25 2] [2 90 150])))
    (is (thrown? IllegalArgumentException
                 (mt/mincost-tickets [1 2] [2 0.9 150])))
    (is (thrown? IllegalArgumentException (mt/mincost-tickets [-1 2] [2 90 150])))
    (is (thrown? IllegalArgumentException (mt/mincost-tickets [2 367] [2 90 150])))
    (is (thrown? IllegalArgumentException (mt/mincost-tickets [2 3] [])))
    (is (thrown? IllegalArgumentException (mt/mincost-tickets [] [])))
    (is (thrown? IllegalArgumentException (mt/mincost-tickets [2 3] [1 2 3 4])))))
