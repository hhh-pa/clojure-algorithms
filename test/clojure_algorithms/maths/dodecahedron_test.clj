(ns clojure-algorithms.maths.dodecahedron-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.dodecahedron :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(deftest test-fn-dodecahedron-surface-area-examples
  (is (approx 516.1432201766901 (sut/dodecahedron-surface-area 5)))
  (is (approx 2064.5728807067603 (sut/dodecahedron-surface-area 10)))
  (is (thrown? Exception (sut/dodecahedron-surface-area -1))))

(deftest test-fn-dodecahedron-volume-examples
  (is (approx 957.8898700780791 (sut/dodecahedron-volume 5)))
  (is (approx 7663.118960624633 (sut/dodecahedron-volume 10)))
  (is (thrown? Exception (sut/dodecahedron-volume -1))))
