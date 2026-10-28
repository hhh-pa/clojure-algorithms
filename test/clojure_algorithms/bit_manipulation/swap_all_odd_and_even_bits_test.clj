(ns clojure-algorithms.bit-manipulation.swap-all-odd-and-even-bits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.swap-all-odd-and-even-bits
             :refer [swap-odd-even-bits show-bits]]
            [clojure.string :as str]))

(deftest swap-odd-even-bits-test
  (testing "doctest examples"
    (is (= 0 (swap-odd-even-bits 0)))
    (is (= 2 (swap-odd-even-bits 1)))
    (is (= 1 (swap-odd-even-bits 2)))
    (is (= 3 (swap-odd-even-bits 3)))
    (is (= 8 (swap-odd-even-bits 4)))
    (is (= 10 (swap-odd-even-bits 5)))
    (is (= 9 (swap-odd-even-bits 6)))
    (is (= 43 (swap-odd-even-bits 23))))
  (testing "swap is an involution"
    (doseq [n [0 1 7 23 100 255]]
      (is (= n (swap-odd-even-bits (swap-odd-even-bits n)))))))

(deftest show-bits-test
  (is (= "    0: 00000000\n    0: 00000000"
         (str/replace (show-bits 0 0) (System/lineSeparator) "\n")))
  (is (= "   23: 00010111\n   43: 00101011"
         (str/replace (show-bits 23 43) (System/lineSeparator) "\n"))))
