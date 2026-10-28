(ns clojure-algorithms.maths.area-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.area :as sut]))

(deftest test-fn-surface-area-cube-examples
  (is (approx 6 (sut/surface-area-cube 1)))
  (is (approx 15.360000000000003 (sut/surface-area-cube 1.6)))
  (is (approx 0 (sut/surface-area-cube 0)))
  (is (approx 54 (sut/surface-area-cube 3)))
  (is (thrown? Exception (sut/surface-area-cube -1))))

(deftest test-fn-surface-area-cuboid-examples
  (is (approx 22 (sut/surface-area-cuboid 1 2 3)))
  (is (approx 0 (sut/surface-area-cuboid 0 0 0)))
  (is (approx 38.56 (sut/surface-area-cuboid 1.6 2.6 3.6)))
  (is (thrown? Exception (sut/surface-area-cuboid -1 2 3)))
  (is (thrown? Exception (sut/surface-area-cuboid 1 -2 3)))
  (is (thrown? Exception (sut/surface-area-cuboid 1 2 -3))))

(deftest test-fn-surface-area-sphere-examples
  (is (approx 314.1592653589793 (sut/surface-area-sphere 5)))
  (is (approx 12.566370614359172 (sut/surface-area-sphere 1)))
  (is (approx 32.169908772759484 (sut/surface-area-sphere 1.6)))
  (is (approx 0.0 (sut/surface-area-sphere 0)))
  (is (thrown? Exception (sut/surface-area-sphere -1))))

(deftest test-fn-surface-area-hemisphere-examples
  (is (approx 235.61944901923448 (sut/surface-area-hemisphere 5)))
  (is (approx 9.42477796076938 (sut/surface-area-hemisphere 1)))
  (is (approx 0.0 (sut/surface-area-hemisphere 0)))
  (is (approx 11.40398133253095 (sut/surface-area-hemisphere 1.1)))
  (is (thrown? Exception (sut/surface-area-hemisphere -1))))

(deftest test-fn-surface-area-cone-examples
  (is (approx 1130.9733552923256 (sut/surface-area-cone 10 24)))
  (is (approx 301.59289474462014 (sut/surface-area-cone 6 8)))
  (is (approx 23.387862992395807 (sut/surface-area-cone 1.6 2.6)))
  (is (approx 0.0 (sut/surface-area-cone 0 0)))
  (is (thrown? Exception (sut/surface-area-cone -1 -2)))
  (is (thrown? Exception (sut/surface-area-cone 1 -2)))
  (is (thrown? Exception (sut/surface-area-cone -1 2))))

(deftest test-fn-lateral-surface-cone-examples
  (is (approx 702.4814731040726 (sut/lateral-surface-cone 10 20)))
  (is (approx 188.49555921538757 (sut/lateral-surface-cone 6 8)))
  (is (approx 0.0 (sut/lateral-surface-cone 0 0)))
  (is (thrown? Exception (sut/lateral-surface-cone 1 -2)))
  (is (thrown? Exception (sut/lateral-surface-cone -1 2))))

(deftest test-fn-surface-area-conical-frustum-examples
  (is (approx 45.511728065337266 (sut/surface-area-conical-frustum 1 2 3)))
  (is (approx 300.7913575056268 (sut/surface-area-conical-frustum 4 5 6)))
  (is (approx 0.0 (sut/surface-area-conical-frustum 0 0 0)))
  (is (approx 78.57907060751548 (sut/surface-area-conical-frustum 1.6 2.6 3.6)))
  (is (thrown? Exception (sut/surface-area-conical-frustum -1 2 3)))
  (is (thrown? Exception (sut/surface-area-conical-frustum 1 -2 3)))
  (is (thrown? Exception (sut/surface-area-conical-frustum 1 2 -3))))

(deftest test-fn-surface-area-cylinder-examples
  (is (approx 747.6990515543707 (sut/surface-area-cylinder 7 10)))
  (is (approx 42.22300526424682 (sut/surface-area-cylinder 1.6 2.6)))
  (is (approx 0.0 (sut/surface-area-cylinder 0 0)))
  (is (approx 527.7875658030853 (sut/surface-area-cylinder 6 8)))
  (is (thrown? Exception (sut/surface-area-cylinder -1 -2))))

(deftest test-fn-lateral-surface-cylinder-examples
  (is (approx 1256.6370614359173 (sut/lateral-surface-cylinder 10 20))))

(deftest test-fn-surface-area-torus-examples
  (is (approx 39.47841760435743 (sut/surface-area-torus 1 1))))

(deftest test-fn-area-rectangle-examples
  (is (approx 200 (sut/area-rectangle 10 20))))

(deftest test-fn-area-square-examples
  (is (approx 100 (sut/area-square 10))))

(deftest test-fn-area-triangle-examples
  (is (approx 50.0 (sut/area-triangle 10 10))))

(deftest test-fn-area-triangle-three-sides-examples
  (is (approx 30.0 (sut/area-triangle-three-sides 5 12 13))))

(deftest test-fn-area-parallelogram-examples
  (is (approx 200 (sut/area-parallelogram 10 20))))

(deftest test-fn-area-trapezium-examples
  (is (approx 450.0 (sut/area-trapezium 10 20 30))))

(deftest test-fn-area-circle-examples
  (is (approx 1256.6370614359173 (sut/area-circle 20))))

(deftest test-fn-area-ellipse-examples
  (is (approx 314.1592653589793 (sut/area-ellipse 10 10))))

(deftest test-fn-area-rhombus-examples
  (is (approx 100.0 (sut/area-rhombus 10 20))))

(deftest test-fn-area-reg-polygon-examples
  (is (approx 43.301270189221945 (sut/area-reg-polygon 3 10))))

(deftest test-fn-surface-area-reg-prism-examples
  (is (approx 686.6025403784439 (sut/surface-area-reg-prism 3 10 20))))
