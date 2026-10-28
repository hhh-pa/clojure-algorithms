(ns clojure-algorithms.maths.derangement
  "A Python implementation for finding number of\nderangements possible for k objects\nhttps://en.wikipedia.org/wiki/Derangement"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare derangement)

(defn derangement "Calculates the number of derangements of k objects.\n:param objects:the number of objects ( -1 < objects < 1560 )\n:return :the number of derangements\n:raises :ValueError: If objects is negative.\n\nExamples:\n>>> derangement(3)\n2\n>>> derangement(5)\n44\n>>> derangement(10)\n1334961"
  [objects] (let [answer (atom nil) derange-1 (atom nil) derange-2 (atom nil) i (atom nil)] (try (when (py-truthy (py-lt objects 0)) (throw (ex-info (py-str "k must be a non-negative integer. Retry") {:type :ValueError}))) (when (py-truthy (py-in objects [0 1])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (reset! derange-1 1) (reset! derange-2 0) (reset! answer 1) (doseq [__i1 (py-iter (range 3 (py-add objects 1)))] (reset! i __i1) (reset! answer (py-mul (py-sub @i 1) (py-add @derange-1 @derange-2))) (reset! derange-2 @derange-1) (reset! derange-1 @answer)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @answer})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
