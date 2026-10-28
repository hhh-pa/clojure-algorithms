(ns clojure-algorithms.maths.series.alternating-harmonic-series
  "Alternating Harmonic Series Checker"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare is-alternate-harmonic-series)

(defn is-alternate-harmonic-series "Returns True if the given series is an Alternating Harmonic Series.\n\nExamples:\n>>> is_alternate_harmonic_series([1, -1/2, 1/3, -1/4])\nTrue\n\n>>> is_alternate_harmonic_series([1, 1/2, 1/3])\nFalse\n\n>>> is_alternate_harmonic_series([])\nFalse"
  [series] (let [expected (atom nil) i (atom nil) value (atom nil)] (try (when (py-truthy (not (py-truthy series))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (doseq [[__i1 __i2] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) series))] (reset! i __i1) (reset! value __i2) (reset! expected (py-mul (py-pow (py-neg 1) @i) (py-div 1 (py-add @i 1)))) (when (py-truthy (py-gt (py-abs (py-sub @value @expected)) 1e-09)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
