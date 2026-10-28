(ns clojure-algorithms.maths.joint-probability-distribution
  "Calculate joint probability distribution\nhttps://en.wikipedia.org/wiki/Joint_probability_distribution"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare joint-probability-distribution expectation variance covariance standard-deviation)

(defn joint-probability-distribution ">>> joint_distribution =  joint_probability_distribution(\n...     [1, 2], [-2, 5, 8], [0.7, 0.3], [0.3, 0.5, 0.2]\n... )\n>>> from math import isclose\n>>> isclose(joint_distribution.pop((1, 8)), 0.14)\nTrue\n>>> joint_distribution\n{(1, -2): 0.21, (1, 5): 0.35, (2, -2): 0.09, (2, 5): 0.15, (2, 8): 0.06}"
  [x-values y-values x-probabilities y-probabilities] (into {} (for [[x x-prob] (py-iter (map vector x-values x-probabilities)) [y y-prob] (py-iter (map vector y-values y-probabilities))] (list [[x y] (py-mul x-prob y-prob)]))))

(defn expectation ">>> from math import isclose\n>>> isclose(expectation([1, 2], [0.7, 0.3]), 1.3)\nTrue"
  [values probabilities] (reduce +' 0 (doall (for [[x p] (py-iter (map vector values probabilities))] (py-mul x p)))))

(defn variance ">>> from math import isclose\n>>> isclose(variance([1,2],[0.7,0.3]), 0.21)\nTrue"
  [values probabilities] (let [mean (atom nil)] (try (reset! mean (expectation values probabilities)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [[x p] (py-iter (map vector values probabilities))] (py-mul (py-pow (py-sub x @mean) 2) p))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn covariance ">>> covariance([1, 2], [-2, 5, 8], [0.7, 0.3], [0.3, 0.5, 0.2])\n-2.7755575615628914e-17"
  [x-values y-values x-probabilities y-probabilities] (let [mean-x (atom nil) mean-y (atom nil)] (try (reset! mean-x (expectation x-values x-probabilities)) (reset! mean-y (expectation y-values y-probabilities)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [[x px] (py-iter (map vector x-values x-probabilities)) [y py] (py-iter (map vector y-values y-probabilities))] (py-mul (py-mul (py-mul (py-sub x @mean-x) (py-sub y @mean-y)) px) py))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn standard-deviation ">>> standard_deviation(0.21)\n0.458257569495584"
  [variance] (py-pow variance 0.5))
