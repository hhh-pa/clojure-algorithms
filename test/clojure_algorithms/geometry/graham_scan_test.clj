(ns clojure-algorithms.geometry.graham-scan-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.graham-scan
             :refer [->Point euclidean-distance consecutive-orientation
                     graham-scan]]))

(deftest test-point-helpers
  (is (= 5.0 (euclidean-distance (->Point 0 0) (->Point 3 4))))
  (is (= 1 (consecutive-orientation (->Point 0 0) (->Point 1 0) (->Point 1 1))))
  (is (= -1 (consecutive-orientation (->Point 0 0) (->Point 1 0) (->Point 1 -1))))
  (is (= 0 (consecutive-orientation (->Point 0 0) (->Point 1 0) (->Point 2 0)))))

(deftest test-graham-scan
  (is (= [] (graham-scan [])))
  (is (= [] (graham-scan [(->Point 0 0)])))
  (is (= [] (graham-scan [(->Point 0 0) (->Point 1 1)])))
  (let [tri [(->Point 0 0) (->Point 1 0) (->Point 0.5 1)]
        hull (set (graham-scan tri))]
    (is (= 3 (count hull)))
    (is (= (set tri) hull)))
  (let [hull (set (graham-scan [(->Point 0 0) (->Point 2 0) (->Point 2 2)
                                (->Point 0 2) (->Point 1 1)]))]
    (is (= #{(->Point 0 0) (->Point 2 0) (->Point 2 2) (->Point 0 2)} hull))))
