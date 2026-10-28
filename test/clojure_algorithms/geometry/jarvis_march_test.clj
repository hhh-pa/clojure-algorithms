(ns clojure-algorithms.geometry.jarvis-march-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.jarvis-march
             :refer [->Point jarvis-march]]))

(deftest test-jarvis-march
  (is (= [] (jarvis-march [])))
  (is (= [] (jarvis-march [(->Point 0 0) (->Point 1 1)])))
  (let [hull (set (jarvis-march [(->Point 0 0) (->Point 1 1) (->Point 0 1)
                                 (->Point 1 0) (->Point 0.5 0.5)]))]
    (is (= #{(->Point 0 0) (->Point 1 0) (->Point 1 1) (->Point 0 1)} hull)))
  (is (= [] (jarvis-march [(->Point 0 0) (->Point 1 1) (->Point 2 2)]))))
