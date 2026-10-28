(ns clojure-algorithms.data-structures.suffix-tree.example.example-usage-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.suffix-tree.example.example-usage
             :refer [run-demo]]))

(deftest test-run-demo
  (is (= {"ana" true "ban" true "na" true "xyz" false "mon" true}
         (run-demo))))
