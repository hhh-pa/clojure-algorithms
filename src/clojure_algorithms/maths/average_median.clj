(ns clojure-algorithms.maths.average-median
  "Port of maths/average_median.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare median main)

(defn median "Find median of a list of numbers.\nWiki: https://en.wikipedia.org/wiki/Median\n\n>>> median([0])\n0\n>>> median([4, 1, 3, 2])\n2.5\n>>> median([2, 70, 6, 50, 20, 8, 4])\n8\n\nArgs:\n    nums: List of nums\n\nReturns:\n    Median."
  [nums] (let [length (atom nil) mid-index (atom nil) sorted-list (atom nil)] (try (reset! sorted-list (py-sorted nums)) (reset! length (count @sorted-list)) (reset! mid-index (.shiftRight (.toBigInteger (bigint @length)) (int 1))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-eq (py-mod @length 2) 0)) (py-div (py-add (py-get @sorted-list @mid-index) (py-get @sorted-list (py-sub @mid-index 1))) 2) (py-get @sorted-list @mid-index))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [& _args]
  ;; stub: original requires method .testmod; pure-Clojure port not applicable
  (throw (ex-info "requires method .testmod" {:type :UnsupportedOperation})))

;; NOTE: main stubbed (method .testmod)
