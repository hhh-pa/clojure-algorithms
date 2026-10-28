(ns clojure-algorithms.data-structures.kd-tree.example.example-usage-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.kd-tree.example.example-usage
             :refer [run-demo]]))

(deftest test-run-demo
  (let [res (run-demo 50 10.0 3)]
    (is (= 3 (count (:query-point res))))
    (is (some? (:nearest-point res)))
    (is (>= (:distance res) 0))
    (is (pos? (:nodes-visited res)))))
