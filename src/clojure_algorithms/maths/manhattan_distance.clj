(ns clojure-algorithms.maths.manhattan-distance
  "Manhattan distance. https://en.wikipedia.org/wiki/Taxicab_geometry"
  (:require [clojure.string :as str]))

(defn- type-name
  "Python-style type name used in error messages."
  [x]
  (cond
    (string? x) "str"
    (integer? x) "int"
    (number? x) "float"
    (boolean? x) "bool"
    (nil? x) "NoneType"
    :else (str/lower-case (.getSimpleName (class x)))))

(defn- missing-input?
  "Mimics Python's falsy check on the point argument."
  [point]
  (or (nil? point)
      (false? point)
      (and (number? point) (zero? point))
      (and (string? point) (empty? point))
      (and (coll? point) (empty? point))))

(defn -validate-point
  "Validates that point is a non-empty list of numbers."
  [point]
  (cond
    (missing-input? point)
    (throw (ex-info "Missing an input" {:type :ValueError}))

    (not (sequential? point))
    (throw (ex-info (str "Expected a list of numbers as input, found "
                         (type-name point))
                    {:type :TypeError}))

    :else
    (when-let [bad (first (remove number? point))]
      (throw (ex-info (str "Expected a list of numbers as input, found "
                           (type-name bad))
                      {:type :TypeError})))))

(defn- manhattan-sum
  "Sums the absolute coordinate differences of two validated points."
  [point-a point-b]
  (-validate-point point-a)
  (-validate-point point-b)
  (when (not= (count point-a) (count point-b))
    (throw (ex-info "Both points must be in the same n-dimensional space"
                    {:type :ValueError})))
  (double (reduce + 0.0
                  (map (fn [a b] (Math/abs (- (double a) (double b))))
                       point-a point-b))))

(defn manhattan-distance
  "Expects two lists of numbers representing two points in the same
n-dimensional space, and returns their Manhattan distance."
  [point-a point-b]
  (manhattan-sum point-a point-b))

(defn manhattan-distance-one-liner
  "One-liner version of manhattan-distance."
  [point-a point-b]
  (manhattan-sum point-a point-b))
