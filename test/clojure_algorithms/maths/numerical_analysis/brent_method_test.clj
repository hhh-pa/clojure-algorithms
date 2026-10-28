(ns clojure-algorithms.maths.numerical-analysis.brent-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.brent-method :as sut]))

(defn- ex-type
  [thunk]
  (try (thunk) :no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-brent-method-examples
  (let [root (sut/brent-method (fn [x] (- (* x x x) x 2)) 1 2)]
    (is (< (Math/abs (- root 1.52138)) 1e-5)))
  (is (= :ValueError
         (ex-type #(sut/brent-method (fn [x] (+ (* x x) 1)) 0 1)))))

(deftest test-brent-method-tolerance
  (let [root (sut/brent-method (fn [x] (- (* x x x) x 2)) 1 2)]
    (is (< (Math/abs (- (* root root root) root 2)) 1e-6))))
