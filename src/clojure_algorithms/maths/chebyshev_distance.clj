(ns clojure-algorithms.maths.chebyshev-distance
  "Port of maths/chebyshev_distance.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare chebyshev-distance)

(defn chebyshev-distance "This function calculates the Chebyshev distance (also known as the\nChessboard distance) between two n-dimensional points represented as lists.\n\nhttps://en.wikipedia.org/wiki/Chebyshev_distance\n\n>>> chebyshev_distance([1.0, 1.0], [2.0, 2.0])\n1.0\n>>> chebyshev_distance([1.0, 1.0, 9.0], [2.0, 2.0, -5.2])\n14.2\n>>> chebyshev_distance([1.0], [2.0, 2.0])\nTraceback (most recent call last):\n    ...\nValueError: Both points must have the same dimension."
  [point-a point-b] (try (when (py-truthy (not (py-eq (count point-a) (count point-b)))) (throw (ex-info (py-str "Both points must have the same dimension.") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-max (doall (for [[a b] (py-iter (map vector point-a point-b))] (py-abs (py-sub a b)))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
