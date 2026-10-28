(ns clojure-algorithms.dynamic-programming.bitmask-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.bitmask :as bm]))

(deftest count-no-of-ways-test
  (testing "task assignment counts"
    (is (= 10 (bm/count-no-of-ways [[1 3 4] [1 2 5] [3 4]] 5)))
    (is (= 1 (bm/count-no-of-ways [[1]] 1)))
    (is (= 0 (bm/count-no-of-ways [[2]] 1)))
    (is (= 1 (bm/count-no-of-ways [[1] [2]] 2)))
    (is (= 0 (bm/count-no-of-ways [[1] [1]] 1)))))
