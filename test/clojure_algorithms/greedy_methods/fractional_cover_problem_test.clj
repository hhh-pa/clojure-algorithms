(ns clojure-algorithms.greedy-methods.fractional-cover-problem-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.fractional-cover-problem
             :refer [->Item fractional-cover ratio]]))

(deftest test-ratio
  (is (= 6.5 (ratio (->Item 10 65))))
  (is (= 5.0 (ratio (->Item 20 100)))))

(deftest test-fractional-cover
  (is (= 240.0 (fractional-cover [(->Item 10 60) (->Item 20 100) (->Item 30 120)] 50)))
  (is (= 135.0 (fractional-cover [(->Item 20 100) (->Item 30 120) (->Item 10 60)] 25)))
  (is (= 280.0 (fractional-cover [(->Item 10 60) (->Item 20 100) (->Item 30 120)] 60)))
  (is (= 180.0 (fractional-cover [(->Item 5 30) (->Item 10 60) (->Item 15 90)] 30)))
  (is (= 0.0 (fractional-cover [] 50)))
  (is (= 30.0 (fractional-cover [(->Item 10 60)] 5)))
  (is (= 6.0 (fractional-cover [(->Item 10 60)] 1)))
  (is (= 0.0 (fractional-cover [(->Item 10 60)] 0)))
  (is (thrown? clojure.lang.ExceptionInfo
               (fractional-cover [(->Item 10 60)] -1))))
