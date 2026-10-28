(ns clojure-algorithms.maths.sum-of-harmonic-series
  "Port of maths/sum_of_harmonic_series.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare sum-of-harmonic-progression)

(defn sum-of-harmonic-progression "https://en.wikipedia.org/wiki/Harmonic_progression_(mathematics)\n\nFind the sum of n terms in an harmonic progression.  The calculation starts with the\nfirst_term and loops adding the common difference of Arithmetic Progression by which\nthe given Harmonic Progression is linked.\n\n>>> sum_of_harmonic_progression(1 / 2, 2, 2)\n0.75\n>>> sum_of_harmonic_progression(1 / 5, 5, 5)\n0.45666666666666667"
  [first-term common-difference number-of-terms] (let [first-term (atom first-term)] (let [--v (atom nil) arithmetic-progression (atom nil) harmonic-series (atom nil)] (try (reset! arithmetic-progression [(py-div 1 @first-term)]) (reset! first-term (py-div 1 @first-term)) (doseq [__i1 (py-iter (range (py-sub number-of-terms 1)))] (reset! --v __i1) (swap! first-term py-add common-difference) (swap! arithmetic-progression conj @first-term)) (reset! harmonic-series (vec (for [step (py-iter @arithmetic-progression)] (py-div 1 step)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 @harmonic-series)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
