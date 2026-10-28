(ns clojure-algorithms.bit-manipulation.numbers-different-signs-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.numbers-different-signs :refer [different-signs]]))

(deftest different-signs-test
  (testing "doctest examples"
    (is (true? (different-signs 1 -1)))
    (is (false? (different-signs 1 1)))
    (is (true? (different-signs 1000000000000000000000000000 -1000000000000000000000000000)))
    (is (true? (different-signs -1000000000000000000000000000 1000000000000000000000000000)))
    (is (false? (different-signs 50 278)))
    (is (false? (different-signs 0 2)))
    (is (false? (different-signs 2 0)))))
