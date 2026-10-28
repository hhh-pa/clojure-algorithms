(ns clojure-algorithms.linear-algebra.src.transformations-2d-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.transformations-2d
             :refer [projection reflection rotation scaling]]))

(defn- mat-approx [a b]
  (every? true? (map (fn [ra rb] (every? true? (map #(< (Math/abs (- %1 %2)) 1e-12) ra rb))) a b)))

(deftest test-transformations-2d
  (is (= [[5.0 0.0] [0.0 5.0]] (scaling 5)))
  (is (mat-approx [[0.5253219888177297 -0.8509035245341184]
                   [0.8509035245341184 0.5253219888177297]]
                  (rotation 45)))
  (is (mat-approx [[0.27596319193541496 0.446998331800279]
                   [0.446998331800279 0.7240368080645851]]
                  (projection 45)))
  (is (mat-approx [[0.05064397763545947 0.893996663600558]
                   [0.893996663600558 0.7018070490682369]]
                  (reflection 45))))
