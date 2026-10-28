(ns clojure-algorithms.maths.joint-probability-distribution
  "Calculate joint probability distribution
https://en.wikipedia.org/wiki/Joint_probability_distribution")

(defn joint-probability-distribution
  "Joint probability distribution of two independent variables as a map
from [x y] pairs to probabilities."
  [x-values y-values x-probabilities y-probabilities]
  (into {}
        (for [[x x-prob] (map vector x-values x-probabilities)
              [y y-prob] (map vector y-values y-probabilities)]
          [[x y] (* x-prob y-prob)])))

(defn expectation
  "Expectation (mean) of values under probabilities."
  [values probabilities]
  (reduce + 0 (map * values probabilities)))

(defn variance
  "Variance of values under probabilities."
  [values probabilities]
  (let [mean (expectation values probabilities)]
    (reduce + 0 (map (fn [x p]
                       (let [d (- x mean)]
                         (* d d p)))
                     values probabilities))))

(defn covariance
  "Covariance of two independent variables."
  [x-values y-values x-probabilities y-probabilities]
  (let [mean-x (expectation x-values x-probabilities)
        mean-y (expectation y-values y-probabilities)]
    (reduce + 0
            (for [[x px] (map vector x-values x-probabilities)
                  [y py] (map vector y-values y-probabilities)]
              (* (- x mean-x) (- y mean-y) px py)))))

(defn standard-deviation
  "Standard deviation from a variance."
  [variance]
  (Math/sqrt variance))
