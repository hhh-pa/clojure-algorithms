(ns clojure-algorithms.linear-algebra.src.polynom-for-points-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.polynom-for-points :refer [points-to-polynomial]]))

(deftest test-points-to-polynomial
  (is (thrown? clojure.lang.ExceptionInfo (points-to-polynomial [])))
  (is (thrown? clojure.lang.ExceptionInfo (points-to-polynomial [[]])))
  (is (= "f(x)=x^2*0.0+x^1*-0.0+x^0*0.0"
         (points-to-polynomial [[1 0] [2 0] [3 0]])))
  (is (= "f(x)=x^2*0.0+x^1*-0.0+x^0*1.0"
         (points-to-polynomial [[1 1] [2 1] [3 1]])))
  (is (= "f(x)=x^2*0.0+x^1*-0.0+x^0*3.0"
         (points-to-polynomial [[1 3] [2 3] [3 3]])))
  (is (= "f(x)=x^2*0.0+x^1*1.0+x^0*0.0"
         (points-to-polynomial [[1 1] [2 2] [3 3]])))
  (is (= "f(x)=x^2*1.0+x^1*-0.0+x^0*0.0"
         (points-to-polynomial [[1 1] [2 4] [3 9]])))
  (is (= "f(x)=x^2*1.0+x^1*-0.0+x^0*2.0"
         (points-to-polynomial [[1 3] [2 6] [3 11]])))
  (is (= "f(x)=x^2*-1.0+x^1*-0.0+x^0*-2.0"
         (points-to-polynomial [[1 -3] [2 -6] [3 -11]])))
  (is (= "f(x)=x^2*5.0+x^1*-18.0+x^0*18.0"
         (points-to-polynomial [[1 5] [2 2] [3 9]])))
  (is (= "x=1" (points-to-polynomial [[1 1] [1 2] [1 3]])))
  (is (thrown? clojure.lang.ExceptionInfo
               (points-to-polynomial [[1 1] [2 2] [2 2]])))
  (is (= "f(x)=x^2*1.0+x^1*0.0+x^0*1.0"
         (points-to-polynomial [[0 1] [1 2] [2 5]]))))
