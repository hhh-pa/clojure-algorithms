(ns clojure-algorithms.electronics.wheatstone-bridge-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.wheatstone-bridge :as wb]))

(deftest wheatstone-solver-test
  (testing "unknown resistances"
    (is (= 10.0 (wb/wheatstone-solver 2 4 5)))
    (is (< (Math/abs (- (wb/wheatstone-solver 356 234 976) 641.5280898876405))
           1e-9)))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (wb/wheatstone-solver 2 -1 2)))
    (is (thrown? IllegalArgumentException (wb/wheatstone-solver 0 0 2)))))
