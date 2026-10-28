(ns clojure-algorithms.maths.pi-monte-carlo-estimation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.pi-monte-carlo-estimation :as sut]))

(deftest test-point-basics
  (let [p (sut/make-Point 0.5 0.5)]
    (is (= 0.5 (:x p)))
    (is (= 0.5 (:y p)))
    (is (true? (sut/Point-is-in-unit-circle p))))
  (is (false? (sut/Point-is-in-unit-circle (sut/make-Point 1 1))))
  (is (true? (sut/Point-is-in-unit-circle (sut/make-Point 1 0))))
  (is (true? (sut/Point-is-in-unit-circle (sut/make-Point 0 0)))))

(deftest test-point-random-unit-square
  (let [r (sut/rng 1)
        p (sut/Point-random-unit-square r)]
    (is (approx 0.13436424411240122 (:x p)))
    (is (approx 0.8474337369372327 (:y p))))
  (is (approx 0.6307877176639078
              (:x (sut/Point-random-unit-square (sut/rng "x")))))
  (let [r (sut/rng 42)]
    (is (every? true?
                (repeatedly 20 (fn [] (let [p (sut/Point-random-unit-square r)]
                                        (and (<= 0.0 (:x p) 1.0)
                                             (<= 0.0 (:y p) 1.0)))))))))

(deftest test-fn-estimate-pi-examples
  (is (= 3.2 (sut/estimate-pi 100 1)))
  (is (= 3.156 (sut/estimate-pi 1000 11)))
  (is (= 3.139892 (sut/estimate-pi 1000000 111))))

(deftest test-fn-estimate-pi-extra
  (is (thrown? Exception (sut/estimate-pi 0)))
  (is (thrown? Exception (sut/estimate-pi -5)))
  (is (= :ValueError (try (sut/estimate-pi 0)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= (sut/estimate-pi 1000 11) (sut/estimate-pi 1000 11)))
  (let [est (sut/estimate-pi 1000)]
    (is (number? est))
    (is (<= 0.0 est 4.0))))
