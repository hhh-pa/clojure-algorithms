(ns clojure-algorithms.maths.combinations
  "https://en.wikipedia.org/wiki/Combination"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare combinations)

(defn combinations "Returns the number of different combinations of k length which can\nbe made from n values, where n >= k.\n\nExamples:\n>>> combinations(10,5)\n252\n\n>>> combinations(6,3)\n20\n\n>>> combinations(20,5)\n15504\n\n>>> combinations(52, 5)\n2598960\n\n>>> combinations(0, 0)\n1\n\n>>> combinations(-4, -5)\n...\nTraceback (most recent call last):\nValueError: Please enter positive integers for n and k where n >= k"
  [n k] (let [i (atom nil) res (atom nil)] (try (when (py-truthy (let [__b1 (py-lt n k)] (if (py-truthy __b1) __b1 (py-lt k 0)))) (throw (ex-info (py-str "Please enter positive integers for n and k where n >= k") {:type :ValueError}))) (reset! res 1) (doseq [__i2 (py-iter (range k))] (reset! i __i2) (swap! res py-mul (py-sub n @i)) (swap! res py-floordiv (py-add @i 1))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
