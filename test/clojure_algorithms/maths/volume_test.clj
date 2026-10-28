(ns clojure-algorithms.maths.volume-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.volume :as sut]))

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

(deftest test-fn-vol-cube-examples
  (is (approx 1.0 (sut/vol-cube 1)))
  (is (approx 27.0 (sut/vol-cube 3)))
  (is (approx 0.0 (sut/vol-cube 0)))
  (is (approx 4.096000000000001 (sut/vol-cube 1.6)))
  (is (thrown? Exception (sut/vol-cube -1))))

(deftest test-fn-vol-spherical-cap-examples
  (is (approx 5.235987755982988 (sut/vol-spherical-cap 1 2)))
  (is (approx 16.621119532592402 (sut/vol-spherical-cap 1.6 2.6)))
  (is (approx 0.0 (sut/vol-spherical-cap 0 0)))
  (is (thrown? Exception (sut/vol-spherical-cap -1 2)))
  (is (thrown? Exception (sut/vol-spherical-cap 1 -2))))

(deftest test-fn-vol-spheres-intersect-examples
  (is (approx 21.205750411731103 (sut/vol-spheres-intersect 2 2 1)))
  (is (approx 40.71504079052372 (sut/vol-spheres-intersect 2.6 2.6 1.6)))
  (is (approx 0.0 (sut/vol-spheres-intersect 0 0 0)))
  (is (thrown? Exception (sut/vol-spheres-intersect -2 2 1)))
  (is (thrown? Exception (sut/vol-spheres-intersect 2 -2 1)))
  (is (thrown? Exception (sut/vol-spheres-intersect 2 2 -1))))

(deftest test-fn-vol-spheres-union-examples
  (is (approx 45.814892864851146 (sut/vol-spheres-union 2 2 1)))
  (is (approx 48.77802773671288 (sut/vol-spheres-union 1.56 2.2 1.4)))
  (is (thrown? Exception (sut/vol-spheres-union 0 2 1)))
  (is (thrown? Exception (sut/vol-spheres-union "1.56" "2.2" "1.4")))
  (is (thrown? Exception (sut/vol-spheres-union 1 nil 1))))

(deftest test-fn-vol-cuboid-examples
  (is (approx 1.0 (sut/vol-cuboid 1 1 1)))
  (is (approx 6.0 (sut/vol-cuboid 1 2 3)))
  (is (approx 14.976 (sut/vol-cuboid 1.6 2.6 3.6)))
  (is (approx 0.0 (sut/vol-cuboid 0 0 0)))
  (is (thrown? Exception (sut/vol-cuboid -1 2 3)))
  (is (thrown? Exception (sut/vol-cuboid 1 -2 3)))
  (is (thrown? Exception (sut/vol-cuboid 1 2 -3))))

(deftest test-fn-vol-cone-examples
  (is (approx 10.0 (sut/vol-cone 10 3)))
  (is (approx 0.3333333333333333 (sut/vol-cone 1 1)))
  (is (approx 0.8533333333333335 (sut/vol-cone 1.6 1.6)))
  (is (approx 0.0 (sut/vol-cone 0 0)))
  (is (thrown? Exception (sut/vol-cone -1 1)))
  (is (thrown? Exception (sut/vol-cone 1 -1))))

(deftest test-fn-vol-right-circ-cone-examples
  (is (approx 12.566370614359172 (sut/vol-right-circ-cone 2 3)))
  (is (approx 0.0 (sut/vol-right-circ-cone 0 0)))
  (is (approx 4.289321169701265 (sut/vol-right-circ-cone 1.6 1.6)))
  (is (thrown? Exception (sut/vol-right-circ-cone -1 1)))
  (is (thrown? Exception (sut/vol-right-circ-cone 1 -1))))

(deftest test-fn-vol-prism-examples
  (is (approx 20.0 (sut/vol-prism 10 2)))
  (is (approx 11.0 (sut/vol-prism 11 1)))
  (is (approx 2.5600000000000005 (sut/vol-prism 1.6 1.6)))
  (is (approx 0.0 (sut/vol-prism 0 0)))
  (is (thrown? Exception (sut/vol-prism -1 1)))
  (is (thrown? Exception (sut/vol-prism 1 -1))))

(deftest test-fn-vol-pyramid-examples
  (is (approx 10.0 (sut/vol-pyramid 10 3))))

(deftest test-fn-vol-sphere-examples
  (is (approx 523.5987755982989 (sut/vol-sphere 5))))

(deftest test-fn-vol-hemisphere-examples
  (is (approx 2.0943951023931953 (sut/vol-hemisphere 1))))

(deftest test-fn-vol-circular-cylinder-examples
  (is (approx 3.141592653589793 (sut/vol-circular-cylinder 1 1))))

(deftest test-fn-vol-hollow-circular-cylinder-examples
  (is (approx 28.274333882308138 (sut/vol-hollow-circular-cylinder 1 2 3))))

(deftest test-fn-vol-conical-frustum-examples
  (is (approx 48490.482608158454 (sut/vol-conical-frustum 45 7 28))))

(deftest test-fn-vol-torus-examples
  (is (approx 19.739208802178716 (sut/vol-torus 1 1))))

(deftest test-fn-vol-octahedron-examples
  (is (approx 0.0 (sut/vol-octahedron 0))))

(deftest test-fn-vol-icosahedron-examples
  (is (approx 0.0 (sut/vol-icosahedron 0))))
