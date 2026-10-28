(ns clojure-algorithms.geometry.segment-intersection-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.segment-intersection
             :refer [->Point direction on-segment? segments-intersect?]]))

(deftest test-direction
  (is (= -1 (direction (->Point 0 0) (->Point 1 0) (->Point 0 1))))
  (is (= 1 (direction (->Point 0 0) (->Point 0 1) (->Point 1 0))))
  (is (= 0 (direction (->Point 0 0) (->Point 1 1) (->Point 2 2)))))

(deftest test-segments-intersect
  (let [pt ->Point]
    (is (segments-intersect? (pt 0 0) (pt 2 2) (pt 0 2) (pt 2 0)))
    (is (segments-intersect? (pt 0 0) (pt 2 2) (pt 1 1) (pt 3 3)))
    (is (not (segments-intersect? (pt 0 0) (pt 1 0) (pt 2 0) (pt 3 0))))
    (is (not (segments-intersect? (pt 0 0) (pt 1 1) (pt 1 0) (pt 2 1))))
    (is (segments-intersect? (pt 0 0) (pt 1 0) (pt 1 0) (pt 2 0)))))
