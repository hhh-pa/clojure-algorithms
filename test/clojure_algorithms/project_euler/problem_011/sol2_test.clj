(ns clojure-algorithms.project-euler.problem-011.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-011.sol2
             :refer [largest-product solution]]))

(def tiny-grid
  [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])

(def tiny-content "1 2 3 4\n5 6 7 8\n9 10 11 12\n13 14 15 16")

(deftest largest-product-test
  (is (= 43680 (largest-product tiny-grid))))

(deftest solution-test
  (is (= 43680 (solution tiny-content))))
