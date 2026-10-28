(ns clojure-algorithms.dynamic-programming.max-product-subarray-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.max-product-subarray :as mp]))

(deftest max-product-subarray-test
  (testing "maximum products"
    (is (= 6 (mp/max-product-subarray [2 3 -2 4])))
    (is (= 0 (mp/max-product-subarray [-2 0 -1])))
    (is (= 48 (mp/max-product-subarray [2 3 -2 4 -1])))
    (is (= -1 (mp/max-product-subarray [-1])))
    (is (= 0 (mp/max-product-subarray [0])))
    (is (= 0 (mp/max-product-subarray [])))
    (is (= 0 (mp/max-product-subarray nil))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (mp/max-product-subarray [2 3 -2 4.5 -1])))
    (is (thrown? IllegalArgumentException (mp/max-product-subarray "ABC")))))
