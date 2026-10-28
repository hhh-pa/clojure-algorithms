(ns clojure-algorithms.maths.decimal-to-fraction-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.decimal-to-fraction :as sut]))

(deftest test-fn-decimal-to-fraction-examples
  (is (approx [2 1] (sut/decimal-to-fraction 2)))
  (is (approx [89 1] (sut/decimal-to-fraction 89.0)))
  (is (approx [67 1] (sut/decimal-to-fraction "67")))
  (is (approx [45 1] (sut/decimal-to-fraction "45.0")))
  (is (approx [3 2] (sut/decimal-to-fraction 1.5)))
  (is (approx [25 4] (sut/decimal-to-fraction "6.25")))
  (is (thrown? Exception (sut/decimal-to-fraction "78td")))
  (is (approx [0 1] (sut/decimal-to-fraction 0)))
  (is (approx [-5 2] (sut/decimal-to-fraction -2.5)))
  (is (approx [1 8] (sut/decimal-to-fraction 0.125)))
  (is (approx [4000001 4] (sut/decimal-to-fraction 1000000.25)))
  (is (approx [13333 10000] (sut/decimal-to-fraction 1.3333)))
  (is (approx [123 1] (sut/decimal-to-fraction "1.23e2")))
  (is (approx [1 2] (sut/decimal-to-fraction "0.500"))))
