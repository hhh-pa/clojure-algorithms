(ns clojure-algorithms.bit-manipulation.fast-walsh-hadamard-transform-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.fast-walsh-hadamard-transform
             :refer [fwht-xor xor-convolution fwht-or or-convolution
                     fwht-and and-convolution]]))

(deftest fwht-xor-test
  (is (= [10 -2 -4 0] (fwht-xor [1 2 3 4])))
  (is (= [1 2 3 4] (fwht-xor [10 -2 -4 0] true)))
  (is (thrown? clojure.lang.ExceptionInfo (fwht-xor [1 2 3])))
  (is (thrown? clojure.lang.ExceptionInfo (fwht-xor [])))
  (is (thrown? clojure.lang.ExceptionInfo (fwht-xor [1 2 3 5] true))))

(deftest xor-convolution-test
  (is (= [11 10] (xor-convolution [1 2] [3 4])))
  (is (thrown? clojure.lang.ExceptionInfo (xor-convolution [1 2] [3]))))

(deftest fwht-or-test
  (is (= [1 3] (fwht-or [1 2])))
  (is (= [1 2] (fwht-or [1 3] true)))
  (is (thrown? clojure.lang.ExceptionInfo (fwht-or [1 2 3]))))

(deftest or-convolution-test
  (is (= [3 18] (or-convolution [1 2] [3 4])))
  (is (thrown? clojure.lang.ExceptionInfo (or-convolution [1 2] [3]))))

(deftest fwht-and-test
  (is (= [3 2] (fwht-and [1 2])))
  (is (= [1 2] (fwht-and [3 2] true)))
  (is (thrown? clojure.lang.ExceptionInfo (fwht-and [1 2 3]))))

(deftest and-convolution-test
  (is (= [13 8] (and-convolution [1 2] [3 4])))
  (is (thrown? clojure.lang.ExceptionInfo (and-convolution [1 2] [3]))))

(deftest round-trip-test
  (testing "forward then inverse restores the input"
    (let [xs [5 1 7 3 2 8 6 4]]
      (is (= xs (fwht-xor (fwht-xor xs) true)))
      (is (= xs (fwht-or (fwht-or xs) true)))
      (is (= xs (fwht-and (fwht-and xs) true))))))
