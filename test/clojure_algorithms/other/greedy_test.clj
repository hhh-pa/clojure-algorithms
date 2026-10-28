(ns clojure-algorithms.other.greedy-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.greedy :as g]))

(def ^:private foods
  (g/build-menu ["Burger" "Pizza" "Coca Cola" "Rice"
                 "Sambhar" "Chicken" "Fries" "Milk"]
                [80 100 60 70 50 110 90 60]
                [40 60 40 70 100 85 55 70]))

(deftest greedy-test
  (testing "build-menu"
    (is (= 8 (count foods)))
    (is (= {:name "Burger" :value 80 :weight 40} (first foods))))
  (testing "greedy by value with budget 500"
    (let [[taken total] (g/greedy foods 500 :value)]
      (is (= 570.0 total))
      (is (= ["Chicken" "Pizza" "Fries" "Burger" "Rice" "Coca Cola" "Milk"]
             (mapv :name taken)))))
  (testing "nothing fits"
    (is (= [[] 0.0] (g/greedy foods 10 :value)))))
