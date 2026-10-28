(ns clojure-algorithms.maths.euclidean-distance
  "Port of maths/euclidean_distance.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare euclidean-distance euclidean-distance-no-np Vector VectorOut)

(defn euclidean-distance "Calculate the distance between the two endpoints of two vectors.\nA vector is defined as a list, tuple, or numpy 1D array.\n>>> float(euclidean_distance((0, 0), (2, 2)))\n2.8284271247461903\n>>> float(euclidean_distance(np.array([0, 0, 0]), np.array([2, 2, 2])))\n3.4641016151377544\n>>> float(euclidean_distance(np.array([1, 2, 3, 4]), np.array([5, 6, 7, 8])))\n8.0\n>>> float(euclidean_distance([1, 2, 3, 4], [5, 6, 7, 8]))\n8.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn euclidean-distance-no-np "Calculate the distance between the two endpoints of two vectors without numpy.\nA vector is defined as a list, tuple, or numpy 1D array.\n>>> euclidean_distance_no_np((0, 0), (2, 2))\n2.8284271247461903\n>>> euclidean_distance_no_np([1, 2, 3, 4], [5, 6, 7, 8])\n8.0"
  [vector-1 vector-2] (py-pow (reduce +' 0 (doall (for [[v1 v2] (py-iter (map vector vector-1 vector-2))] (py-pow (py-sub v1 v2) 2)))) (py-div 1 2)))

(def Vector nil)

(def VectorOut nil)

;; NOTE: euclidean_distance stubbed (np,numpy-attr:sum)
;; NOTE: const Vector stubbed (np)
;; NOTE: const VectorOut stubbed (np)
