(ns clojure-algorithms.maths.numerical-analysis.adams-bashforth-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure-algorithms.maths.numerical-analysis.adams-bashforth :as sut]))

(defn- ex-type
  [thunk]
  (try (thunk) :no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-constructor
  (let [f (fn [x y] (+ x y))
        m (sut/make-AdamsBashforth f [0 0.2 0.4] [0 0.2 1] 0.2 1)]
    (is (= [0 0.2 0.4] (:x-initials m)))
    (is (= [0 0.2 1] (:y-initials m)))
    (is (= 0.2 (:step-size m)))
    (is (= 1 (:x-final m))))
  (is (= :ValueError
         (ex-type #(sut/make-AdamsBashforth (fn [x y] (+ x y))
                                            [0 0.2 1] [0 0 0.04] 0.2 1))))
  (is (= :ValueError
         (ex-type #(sut/make-AdamsBashforth (fn [x y] (+ x y))
                                            [0 0.2 0.3] [0 0 0.04] 0.2 1))))
  (is (= :ValueError
         (ex-type #(sut/make-AdamsBashforth (fn [x y] (+ x y))
                                            [0 0.2 0.4 0.6 0.8]
                                            [0 0 0.04 0.128 0.307] -0.2 1)))))

(deftest test-step-2
  (let [f (fn [x y] x)
        y (sut/AdamsBashforth-step-2
           (sut/make-AdamsBashforth f [0 0.2] [0 0] 0.2 1))]
    (is (approx [0.0 0.0 0.06 0.16 0.3 0.48] y)))
  (is (= :ValueError
         (ex-type #(sut/AdamsBashforth-step-2
                    (sut/make-AdamsBashforth (fn [x y] x)
                                             [0 0.2 0.4] [0 0 0.04] 0.2 1))))))

(deftest test-step-3
  (let [f (fn [x y] (+ x y))
        y (sut/AdamsBashforth-step-3
           (sut/make-AdamsBashforth f [0 0.2 0.4] [0 0 0.04] 0.2 1))]
    (is (approx 0.15533333333333332 (nth y 3))))
  (is (= :ValueError
         (ex-type #(sut/AdamsBashforth-step-3
                    (sut/make-AdamsBashforth (fn [x y] (+ x y))
                                             [0 0.2] [0 0] 0.2 1))))))

(deftest test-step-4
  (let [f (fn [x y] (+ x y))
        y (sut/AdamsBashforth-step-4
           (sut/make-AdamsBashforth f [0 0.2 0.4 0.6] [0 0 0.04 0.128]
                                    0.2 1))]
    (is (approx 0.30699999999999994 (nth y 4)))
    (is (approx 0.5771083333333333 (nth y 5))))
  (is (= :ValueError
         (ex-type #(sut/AdamsBashforth-step-4
                    (sut/make-AdamsBashforth (fn [x y] (+ x y))
                                             [0 0.2 0.4] [0 0 0.04] 0.2 1))))))

(deftest test-step-5
  (let [f (fn [x y] (+ x y))
        y (sut/AdamsBashforth-step-5
           (sut/make-AdamsBashforth f [0 0.2 0.4 0.6 0.8]
                                    [0 0.02140 0.02140 0.22211 0.42536]
                                    0.2 1))]
    (is (approx 0.05436839444444452 (last y))))
  (is (= :ValueError
         (ex-type #(sut/AdamsBashforth-step-5
                    (sut/make-AdamsBashforth (fn [x y] (+ x y))
                                             [0 0.2 0.4] [0 0 0.04] 0.2 1))))))
