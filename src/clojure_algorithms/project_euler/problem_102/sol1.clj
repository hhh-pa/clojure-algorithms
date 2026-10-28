(ns clojure-algorithms.project-euler.problem-102.sol1
  "Project Euler 102: triangle containment.

  Find the number of triangles, given as coordinate rows, for which the
  interior contains the origin.

  This port takes the triangles as a string (one comma-separated row of
  six coordinates per line) instead of reading p102_triangles.txt, so
  the pure logic can be tested with tiny data."
  (:require [clojure.string :as str]))

(defn vector-product
  "Returns the 2-d vector product of two vectors."
  [point1 point2]
  (- (* (nth point1 0) (nth point2 1))
     (* (nth point1 1) (nth point2 0))))

(defn contains-origin?
  "Checks if the triangle given by the points A(x1, y1), B(x2, y2),
  C(x3, y3) contains the origin."
  [x1 y1 x2 y2 x3 y3]
  (let [point-a [x1 y1]
        point-a-to-b [(- x2 x1) (- y2 y1)]
        point-a-to-c [(- x3 x1) (- y3 y1)]
        a (/ (- (double (vector-product point-a point-a-to-b)))
             (vector-product point-a-to-c point-a-to-b))
        b (/ (double (vector-product point-a point-a-to-c))
             (vector-product point-a-to-c point-a-to-b))]
    (and (pos? a) (pos? b) (< (+ a b) 1))))

(defn solution
  "Finds the number of triangles in content whose interior contains the
  origin."
  [content]
  (let [triangles (for [line (str/split-lines (str/trim content))]
                    (mapv parse-long (str/split line #",")))]
    (count (filter #(apply contains-origin? %) triangles))))
