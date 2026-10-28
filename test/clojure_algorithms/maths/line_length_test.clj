(ns clojure-algorithms.maths.line-length-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.line-length :as sut]))

(deftest test-fn-line-length-examples
  (is (= "1.414214" (format "%.6f" (sut/line-length identity 0 1 10))))
  (is (= "10.000000" (format "%.6f" (sut/line-length (fn [_] 1) -5.5 4.5))))
  (is (= "69.534930"
         (format "%.6f"
                 (sut/line-length (fn [x]
                                    (+ (Math/sin (* 5 x))
                                       (Math/cos (* 10 x))
                                       (/ (* x x) 10)))
                                  0.0 10.0 10000)))))
