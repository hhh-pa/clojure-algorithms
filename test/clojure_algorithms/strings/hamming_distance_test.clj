(ns clojure-algorithms.strings.hamming-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.hamming-distance :refer [hamming-distance]]))

(deftest test-hamming-distance
  (testing "doctest examples"
    (is (= 0 (hamming-distance "python" "python")))
    (is (= 3 (hamming-distance "karolin" "kathrin")))
    (is (= 5 (hamming-distance "00000" "11111"))))
  (testing "unequal lengths throw"
    (is (thrown? IllegalArgumentException
                 (hamming-distance "karolin" "kath")))))
