(ns clojure-algorithms.maths.double-factorial-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.double-factorial :as sut]))

(defn- prod-range
  "Product of range(i, 0, -2), mirroring the upstream doctest oracle."
  [i]
  (reduce *' 1 (range i 0 -2)))

(deftest test-fn-double-factorial-recursive-examples
  (doseq [i (range 20)]
    (is (approx (prod-range i) (sut/double-factorial-recursive i))
        (str "double-factorial-recursive(" i ")")))
  (is (= 1 (sut/double-factorial-recursive 0)))
  (is (= 1 (sut/double-factorial-recursive 1)))
  (is (= 3840 (sut/double-factorial-recursive 10)))
  (is (= 10395 (sut/double-factorial-recursive 11)))
  (is (thrown? Exception (sut/double-factorial-recursive 0.1)))
  (is (thrown? Exception (sut/double-factorial-recursive -1))))

(deftest test-fn-double-factorial-iterative-examples
  (doseq [i (range 20)]
    (is (approx (prod-range i) (sut/double-factorial-iterative i))
        (str "double-factorial-iterative(" i ")")))
  (is (= 1 (sut/double-factorial-iterative 0)))
  (is (= 1 (sut/double-factorial-iterative 1)))
  (is (= 3840 (sut/double-factorial-iterative 10)))
  (is (= 10395 (sut/double-factorial-iterative 11)))
  (is (thrown? Exception (sut/double-factorial-iterative 0.1)))
  (is (thrown? Exception (sut/double-factorial-iterative -1))))
