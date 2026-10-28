(ns clojure-algorithms.maths.average-mode
  "Port of maths/average_mode.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare mode)

(defn mode "This function returns the mode(Mode as in the measures of\ncentral tendency) of the input data.\n\nThe input list may contain any Datastructure or any Datatype.\n\n>>> mode([2, 3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 2, 2, 2])\n[2]\n>>> mode([3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 2, 2, 2])\n[2]\n>>> mode([3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2])\n[2, 4]\n>>> mode([\"x\", \"y\", \"y\", \"z\"])\n['y']\n>>> mode([\"x\", \"x\" , \"y\", \"y\", \"z\"])\n['x', 'y']"
  [input-list] (let [result (atom nil) y (atom nil)] (try (when (py-truthy (not (py-truthy input-list))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (reset! result (vec (for [value (py-iter input-list)] (py-count input-list value)))) (reset! y (py-max @result)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sorted (set (for [[i value] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @result)) :when (py-truthy (py-eq value @y))] (py-get input-list i))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
