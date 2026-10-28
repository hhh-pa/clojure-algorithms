(ns clojure-algorithms.maths.numerical-analysis.bisection-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure-algorithms.maths.numerical-analysis.bisection :as sut]))

(defn- ex-type
  [thunk]
  (try (thunk) :no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-bisection-examples
  (is (approx 1.0000000149011612
              (sut/bisection (fn [x] (- (* x x x) 1)) -5 5)))
  (is (= :ValueError
         (ex-type #(sut/bisection (fn [x] (- (* x x x) 1)) 2 1000))))
  (is (approx 1.0
              (sut/bisection (fn [x] (+ (* x x) (* -4 x) 3)) 0 2)))
  (is (approx 3.0
              (sut/bisection (fn [x] (+ (* x x) (* -4 x) 3)) 2 4)))
  (is (= :ValueError
         (ex-type #(sut/bisection (fn [x] (+ (* x x) (* -4 x) 3)) 4 1000)))))

(deftest test-fn-f-examples
  (is (approx -1 (sut/f 2)))
  (is (approx -5 (sut/f 0))))
