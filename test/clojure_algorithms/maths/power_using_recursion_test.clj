(ns clojure-algorithms.maths.power-using-recursion-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.power-using-recursion :as sut]))

(defn- expected-pow
  "Exact integer power used as the test oracle."
  [base exponent]
  (nth (iterate (partial *' base) 1) exponent))

(deftest test-fn-power-examples
  (is (approx 81 (sut/power 3 4)))
  (is (approx 1 (sut/power 2 0)))
  (doseq [base (range -10 10), exponent (range 10)]
    (is (= (expected-pow base exponent) (sut/power base exponent))
        (str "power(" base ", " exponent ")")))
  (is (= "a" (str (sut/power "a" 1))))
  (is (thrown? Exception (sut/power "a" 2)))
  (is (thrown? Exception (sut/power "a" "b")))
  (is (thrown? Throwable (sut/power 2 -1)))
  (is (approx 1 (sut/power 0 0)))
  (is (approx 0 (sut/power 0 1)))
  (is (approx 15625 (sut/power 5 6)))
  (is (approx 21914624432020321 (sut/power 23 12))))
