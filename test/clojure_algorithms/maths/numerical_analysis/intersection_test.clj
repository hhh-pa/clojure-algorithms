(ns clojure-algorithms.maths.numerical-analysis.intersection-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.intersection :as sut]))

(defn- ex-type
  [thunk]
  (try (thunk) :no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-intersection-examples
  (is (approx 0.9999999999954654
              (sut/intersection (fn [x] (- (* x x x) 1)) -5 5)))
  (is (= :ZeroDivisionError
         (ex-type #(sut/intersection (fn [x] (- (* x x x) 1)) 5 5))))
  (is (approx 1.0000000000003888
              (sut/intersection (fn [x] (- (* x x x) 1)) 100 200)))
  (is (approx 0.9999999998088019
              (sut/intersection (fn [x] (+ (* x x) (* -4 x) 3)) 0 2)))
  (is (approx 2.9999999998088023
              (sut/intersection (fn [x] (+ (* x x) (* -4 x) 3)) 2 4)))
  (is (approx 3.0000000001786042
              (sut/intersection (fn [x] (+ (* x x) (* -4 x) 3)) 4 1000)))
  (is (approx 0.0 (sut/intersection #(Math/sin %) (- Math/PI) Math/PI)))
  (is (= :ZeroDivisionError
         (ex-type #(sut/intersection (fn [x] (Math/cos (double x)))
                                     (- Math/PI) Math/PI)))))

(deftest test-fn-f-examples
  (is (approx -1.0 (sut/f 2))))
