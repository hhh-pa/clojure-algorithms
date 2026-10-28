(ns clojure-algorithms.graphs.johnson-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.johnson :refer [johnson]]))

(def demo {0 [[1 3] [2 8] [4 -4]] 1 [[3 1] [4 7]] 2 [[1 4]]
           3 [[0 2] [2 -5]] 4 [[3 6]]})

(deftest test-johnson
  (is (= 2.0 (get-in (johnson demo) [0 3])))
  (is (= 0.0 (get-in (johnson demo) [2 2])))
  (is (thrown? IllegalArgumentException
               (johnson {0 [[1 1]] 1 [[0 -2]]}))))
