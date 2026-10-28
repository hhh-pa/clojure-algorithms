(ns clojure-algorithms.bit-manipulation.count-number-of-one-bits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.count-number-of-one-bits
             :refer [get-set-bits-count-using-brian-kernighans-algorithm
                     get-set-bits-count-using-modulo-operator
                     get-set-bits-count-using-lookup-table]]))

(deftest set-bits-count-test
  (testing "all three methods agree on doctest examples"
    (doseq [f [get-set-bits-count-using-brian-kernighans-algorithm
               get-set-bits-count-using-modulo-operator
               get-set-bits-count-using-lookup-table]]
      (is (= 3 (f 25)))
      (is (= 3 (f 37)))
      (is (= 3 (f 21)))
      (is (= 4 (f 58)))
      (is (= 0 (f 0)))
      (is (= 1 (f 256)))
      (is (thrown? clojure.lang.ExceptionInfo (f -1)))))
  (testing "extra doctest values"
    (is (= 10 (get-set-bits-count-using-brian-kernighans-algorithm 1023)))
    (is (= 1 (get-set-bits-count-using-modulo-operator 1024)))))
