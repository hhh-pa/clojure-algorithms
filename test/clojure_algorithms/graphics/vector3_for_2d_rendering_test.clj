(ns clojure-algorithms.graphics.vector3-for-2d-rendering-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphics.vector3-for-2d-rendering
             :refer [convert-to-2d rotate]]))

(defn- approx-vec? [a b]
  (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-9) a b)))

(deftest test-convert-to-2d
  (is (approx-vec? (convert-to-2d 1.0 2.0 3.0 10.0 10.0)
                   [7.6923076923076925 15.384615384615385]))
  (is (thrown? IllegalArgumentException (convert-to-2d "1" 2 3 10 10))))

(deftest test-rotate
  (is (approx-vec? (rotate 1.0 2.0 3.0 "y" 90.0)
                   [3.130524675073759 2.0 0.4470070007889556]))
  (is (approx-vec? (rotate 1 2 3 "z" 180)
                   [0.999736015495891 -2.0001319704760485 3]))
  (is (thrown? IllegalArgumentException (rotate "1" 2 3 "z" 90.0)))
  (is (thrown? IllegalArgumentException (rotate 1 2 3 "n" 90))))
