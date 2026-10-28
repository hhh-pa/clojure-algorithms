(ns clojure-algorithms.maths.pythagoras
  "Uses Pythagoras theorem to calculate the distance between two points in space."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare distance make-Point Point-repr)

(defrecord Point [x y z])

(defn make-Point "Constructor for Point."
  [x y z] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:x] x))) (swap! self (fn [__c] (py-assoc-in __c [:y] y))) (swap! self (fn [__c] (py-assoc-in __c [:z] z))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Point @self)))

(defn Point-repr
  [self] (str "Point(" (py-str (:x @self)) ", " (py-str (:y @self)) ", " (py-str (:z @self)) ")"))

(defn distance ">>> point1 = Point(2, -1, 7)\n>>> point2 = Point(1, -3, 5)\n>>> print(f\"Distance from {point1} to {point2} is {distance(point1, point2)}\")\nDistance from Point(2, -1, 7) to Point(1, -3, 5) is 3.0"
  [a b] (Math/sqrt (py-abs (py-add (py-add (py-pow (py-sub (:x b) (:x a)) 2) (py-pow (py-sub (:y b) (:y a)) 2)) (py-pow (py-sub (:z b) (:z a)) 2)))))

(defmethod print-method Point [o w] (.write w (Point-repr (atom o))))
