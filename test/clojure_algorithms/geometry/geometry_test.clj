(ns clojure-algorithms.geometry.geometry-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.geometry :as g]))

(defn- approx? [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

(deftest test-angle-side
  (is (approx? (g/angle-to-radians (g/make-angle 90)) (/ Math/PI 2)))
  (is (= 90.0 (:degrees (g/angle-from-radians (/ Math/PI 2)))))
  (is (thrown? IllegalArgumentException (g/make-angle 361)))
  (is (thrown? IllegalArgumentException (g/make-side -1))))

(deftest test-ellipse-circle
  (is (approx? (g/ellipse-area (g/->Ellipse 5 10)) 157.07963267948966))
  (is (approx? (g/ellipse-perimeter (g/->Ellipse 5 10)) 48.44210548835644))
  (is (approx? (g/ellipse-eccentricity (g/->Ellipse 5 10)) 0.8660254037844386))
  (let [c (g/make-circle 5)]
    (is (= 10 (g/circle-diameter c)))
    (is (= 1.0 (g/max-parts c 0)))
    (is (= 29.0 (g/max-parts c 7)))
    (is (thrown? IllegalArgumentException (g/max-parts c -222)))))

(deftest test-polygon-rectangle
  (let [r (g/make-rectangle 5 10)]
    (is (= 30 (g/rectangle-perimeter r)))
    (is (approx? (g/rectangle-area r) 50))
    (is (= 5 (count (g/get-vertices r)))))
  (is (thrown? IllegalArgumentException (g/make-rectangle -5 10)))
  (let [s (g/make-square 5)]
    (is (= 20 (g/rectangle-perimeter s)))
    (is (approx? (g/rectangle-area s) 25))))

(deftest test-triangle-shape
  (let [t (g/make-triangle 3 4 5)]
    (is (= 12 (g/triangle-perimeter t)))
    (is (approx? (g/triangle-area t) 6.0)))
  (is (= 10.83 (/ (Math/round (* (g/triangle-area (g/make-triangle 5 5 5)) 100))
                  100.0)))
  (is (thrown? IllegalArgumentException (g/make-triangle 1 2 10))))
