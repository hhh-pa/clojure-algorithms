(ns clojure-algorithms.maths.trapezoidal-rule-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.trapezoidal-rule :as sut]))

(deftest test-fn-f-examples
  (is (approx 0 (sut/f 0)))
  (is (approx 1 (sut/f 1)))
  (is (approx 0.25 (sut/f 0.5))))

(deftest test-fn-trapezoidal-rule-examples
  (is (true? (< (Math/abs (- (sut/trapezoidal-rule [0 1] 10) 0.33333)) 0.01)))
  (is (true? (< (Math/abs (- (sut/trapezoidal-rule [0 1] 100) 0.33333)) 0.01)))
  (is (true? (< (Math/abs (- (sut/trapezoidal-rule [0 2] 1000) 2.66667)) 0.01)))
  (is (true? (< (Math/abs (- (sut/trapezoidal-rule [1 2] 1000) 2.33333)) 0.01))))

(deftest test-fn-make-points-examples
  (is (approx [0.1 0.2 0.30000000000000004 0.4 0.5 0.6 0.7
               0.7999999999999999 0.8999999999999999]
              (sut/make-points 0 1 0.1)))
  (is (approx [2.5 5.0 7.5] (sut/make-points 0 10 2.5)))
  (is (approx [2 4 6 8] (sut/make-points 0 10 2)))
  (is (approx [6 11 16] (sut/make-points 1 21 5)))
  (is (approx [3] (sut/make-points 1 5 2)))
  (is (approx [] (sut/make-points 1 4 3))))

(deftest test-fn-main-examples
  (is (= "y = 0.3349999999999999\n" (with-out-str (sut/main)))))
