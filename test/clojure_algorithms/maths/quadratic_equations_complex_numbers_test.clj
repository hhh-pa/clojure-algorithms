(ns clojure-algorithms.maths.quadratic-equations-complex-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.quadratic-equations-complex-numbers :as sut]))

(deftest test-fn-quadratic-roots-examples
  (is (approx [1.0 -4.0] (sut/quadratic-roots 1 3 -4)))
  (is (approx [-0.2 -1.0] (sut/quadratic-roots 5 6 1)))
  (is (approx [[3.0 4.0] [3.0 -4.0]] (sut/quadratic-roots 1 -6 25))))

(deftest test-fn-quadratic-roots-extra
  (is (approx [2.0 1.0] (sut/quadratic-roots 1 -3 2)))
  (is (approx [[0.0 1.0] [0.0 -1.0]] (sut/quadratic-roots 1 0 1)))
  (is (thrown? Exception (sut/quadratic-roots 0 1 1)))
  (is (= :ValueError (try (sut/quadratic-roots 0 1 1)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e)))))))

(deftest test-fn-main
  (is (= "The solutions are: -0.2 and -1.0\n" (with-out-str (sut/main)))))
