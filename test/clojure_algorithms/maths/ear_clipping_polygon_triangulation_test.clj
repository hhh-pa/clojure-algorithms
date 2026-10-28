(ns clojure-algorithms.maths.ear-clipping-polygon-triangulation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.ear-clipping-polygon-triangulation :as sut]))

(deftest test-fn-is-ear-examples
  (is (= false (boolean (sut/is-ear [[0 2] [2 2] [2 0] [1 0] [1 1] [0 1]] 4 "counter-clockwise"))))
  (is (= true (boolean (sut/is-ear [[0 2] [2 2] [2 0] [1 0] [1 1] [0 1]] 3 "counter-clockwise"))))
  (is (= false (boolean (sut/is-ear [[0 3] [2 2] [3 0] [3 3]] 3 "clockwise"))))
  (is (= true (boolean (sut/is-ear [[0 0] [1 0] [1 1] [0 1]] 0 "clockwise")))))

(deftest test-fn-is-convex-examples
  (is (= true (boolean (sut/is-convex [1 1] [2 2] [1 2] "clockwise"))))
  (is (= false (boolean (sut/is-convex [1 1] [2 2] [1 2] "counter-clockwise"))))
  (is (= true (boolean (sut/is-convex [1 1] [2 2] [3 3] "clockwise"))))
  (is (= true (boolean (sut/is-convex [1 1] [2 2] [3 3] "counter-clockwise")))))

(deftest test-fn-cross-product-examples
  (is (approx 0.5 (sut/cross-product [0 0] [1 0] [1.5 0.5])))
  (is (approx -0.5 (sut/cross-product [1.5 0.5] [1 1] [1.5 1.5])))
  (is (approx 0 (sut/cross-product [0 0] [1 1] [2 2]))))

(deftest test-fn-direction-examples
  (is (approx "clockwise" (sut/direction [[1 1] [2 2] [3 4]])))
  (is (approx "counter-clockwise" (sut/direction [[1 1] [2 2] [3 -1]]))))

(deftest test-fn-is-point-inside-triangle-examples
  (is (= false (boolean (sut/is-point-inside-triangle [0 0] [0 2] [2 0] [3 3]))))
  (is (= true (boolean (sut/is-point-inside-triangle [0 0] [0 2] [2 0] [1 1]))))
  (is (= false (boolean (sut/is-point-inside-triangle [0 0] [2 1] [2 0] [1 1]))))
  (is (= true (boolean (sut/is-point-inside-triangle [0 0] [2 1] [2 0] [2 0]))))
  (is (= true (boolean (sut/is-point-inside-triangle [0 0] [1 1] [2 0] [1 0])))))

(deftest test-fn-triangulate-polygon-examples
  (is (approx [[[0 1] [0 2] [2 2]]
               [[2 2] [2 0] [1 0]]
               [[2 2] [1 0] [1 1]]
               [[0 1] [2 2] [1 1]]]
              (sut/triangulate-polygon [[0 2] [2 2] [2 0] [1 0] [1 1] [0 1]])))
  (is (approx [[[3 3] [0 3] [2 2]]
               [[3 3] [2 2] [3 0]]]
              (sut/triangulate-polygon [[0 3] [2 2] [3 0] [3 3]])))
  (is (approx [[[0 0] [2 0] [1 1]]
               [[0 2] [0 0] [1 1]]
               [[0 2] [1 1] [2 2]]]
              (sut/triangulate-polygon [[0 0] [2 0] [1 1] [2 2] [0 2]]))))
