(ns clojure-algorithms.divide-and-conquer.kth-order-statistic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.kth-order-statistic :as k]))

(deftest kth-number-test
  (testing "kth smallest element"
    (is (= 3 (k/kth-number [2 1 3 4 5] 3)))
    (is (= 1 (k/kth-number [2 1 3 4 5] 1)))
    (is (= 5 (k/kth-number [2 1 3 4 5] 5)))
    (is (= 3 (k/kth-number [3 2 5 6 7 8] 2)))
    (is (= 43 (k/kth-number [25 21 98 100 76 22 43 60 89 87] 4)))
    (testing "matches sorting on shuffled input"
      (let [coll (shuffle (range 50))]
        (doseq [kth [1 7 25 49 50]]
          (is (= (nth (sort coll) (dec kth)) (k/kth-number coll kth))))))))
