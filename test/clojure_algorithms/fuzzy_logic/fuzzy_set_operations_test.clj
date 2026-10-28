(ns clojure-algorithms.fuzzy-logic.fuzzy-set-operations-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fuzzy-logic.fuzzy-set-operations
             :refer [triangular-membership fuzzy-union fuzzy-intersection
                     fuzzy-complement fuzzy-difference algebraic-sum
                     algebraic-product bounded-sum bounded-difference]]))

(defn- approx-vec= [a b]
  (and (= (count a) (count b))
       (every? true? (map (fn [x y] (< (Math/abs (- (double x) (double y))) 1e-9))
                          a b))))

(deftest triangular-test
  (is (approx-vec= [0.0 1.0 0.0] (triangular-membership [0.0 25.0 50.0] 0 25 50)))
  (is (approx-vec= [0.4 0.5] (triangular-membership [10.0 12.5] 0 25 50)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"left <= peak <= right"
                        (triangular-membership [1.0] 5 1 2))))

(deftest operators-test
  (is (approx-vec= [0.5 0.7] (fuzzy-union [0.2 0.7] [0.5 0.1])))
  (is (approx-vec= [0.2 0.1] (fuzzy-intersection [0.2 0.7] [0.5 0.1])))
  (is (approx-vec= [1.0 0.7 0.0] (fuzzy-complement [0.0 0.3 1.0])))
  (is (approx-vec= [0.6 0.1] (fuzzy-difference [0.6 0.4] [0.2 0.9])))
  (is (approx-vec= [0.75 1.0] (algebraic-sum [0.5 1.0] [0.5 0.2])))
  (is (approx-vec= [0.25 0.2] (algebraic-product [0.5 1.0] [0.5 0.2])))
  (is (approx-vec= [0.7 1.0] (bounded-sum [0.5 0.8] [0.2 0.7])))
  (is (approx-vec= [0.0 0.5] (bounded-difference [0.5 0.8] [0.2 0.7]))))
