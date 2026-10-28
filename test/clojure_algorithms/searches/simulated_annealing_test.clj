(ns clojure-algorithms.searches.simulated-annealing-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.hill-climbing :as hc]
            [clojure-algorithms.searches.simulated-annealing :as sa]))

(deftest simulated-annealing-test
  (testing "maximize finds a state at least as good as the start"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          start (hc/make-search-problem 3 4 1 f)
          best (sa/simulated-annealing start {:find-max true :seed 1})]
      (is (instance? clojure_algorithms.searches.hill_climbing.SearchProblem
                     best))
      (is (>= (hc/score best) (hc/score start)))))
  (testing "fixed seeds are deterministic"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          opts {:find-max true :seed 7}
          a (sa/simulated-annealing (hc/make-search-problem 3 4 1 f) opts)
          b (sa/simulated-annealing (hc/make-search-problem 3 4 1 f) opts)]
      (is (= a b))))
  (testing "bounded run stays inside the domain"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          best (sa/simulated-annealing (hc/make-search-problem 12 47 1 f)
                                       {:find-max false :max-x 100 :min-x 5
                                        :max-y 50 :min-y -5 :seed 3})]
      (is (<= 5 (:x best) 100))
      (is (<= -5 (:y best) 50)))))
