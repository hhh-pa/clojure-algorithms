(ns clojure-algorithms.maths.pythagoras-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.pythagoras :as sut]))

(deftest test-fn-distance-examples
  (let [point1 (sut/make-Point 2 -1 7)
        point2 (sut/make-Point 1 -3 5)]
    (is (= "Point(2, -1, 7)" (sut/Point-repr point1)))
    (is (= "Point(1, -3, 5)" (sut/Point-repr point2)))
    (is (approx 3.0 (sut/distance point1 point2)))
    (is (= (str "Distance from " (sut/Point-repr point1)
                " to " (sut/Point-repr point2)
                " is " (sut/distance point1 point2))
           "Distance from Point(2, -1, 7) to Point(1, -3, 5) is 3.0"))))
