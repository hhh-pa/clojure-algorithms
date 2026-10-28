(ns clojure-algorithms.maths.two-sum
  "Given an array of integers, return indices of the two numbers such that they add up to\na specific target.\n\nYou may assume that each input would have exactly one solution, and you may not use the\nsame element twice.\n\nExample:\nGiven nums = [2, 7, 11, 15], target = 9,\n\nBecause nums[0] + nums[1] = 2 + 7 = 9,\nreturn [0, 1]."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare two-sum)

(defn two-sum ">>> two_sum([2, 7, 11, 15], 9)\n[0, 1]\n>>> two_sum([15, 2, 11, 7], 13)\n[1, 2]\n>>> two_sum([2, 7, 11, 15], 17)\n[0, 3]\n>>> two_sum([7, 15, 11, 2], 18)\n[0, 2]\n>>> two_sum([2, 7, 11, 15], 26)\n[2, 3]\n>>> two_sum([2, 7, 11, 15], 8)\n[]\n>>> two_sum([3 * i for i in range(10)], 19)\n[]"
  [nums target] (let [chk-map (atom nil) compl (atom nil) index (atom nil) val (atom nil)] (try (reset! chk-map {}) (doseq [[__i1 __i2] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) nums))] (reset! index __i1) (reset! val __i2) (reset! compl (py-sub target @val)) (when (py-truthy (py-in @compl @chk-map)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-get @chk-map @compl) @index]}))) (swap! chk-map (fn [__c] (py-assoc-in __c [@val] @index)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
