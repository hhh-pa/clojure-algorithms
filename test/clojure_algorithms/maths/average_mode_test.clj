(ns clojure-algorithms.maths.average-mode-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.average-mode :as sut]))

(deftest test-fn-mode-examples
  (is (approx [2] (sut/mode [2 3 4 5 3 4 2 5 2 2 4 2 2 2])))
  (is (approx [2] (sut/mode [3 4 5 3 4 2 5 2 2 4 4 2 2 2])))
  (is (approx [2 4] (sut/mode [3 4 5 3 4 2 5 2 2 4 4 4 2 2 4 2])))
  (is (approx ["y"] (sut/mode ["x" "y" "y" "z"])))
  (is (approx ["x" "y"] (sut/mode ["x" "x" "y" "y" "z"])))
  (is (approx [] (sut/mode []))))
