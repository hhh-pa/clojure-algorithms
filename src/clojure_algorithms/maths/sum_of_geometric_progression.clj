(ns clojure-algorithms.maths.sum-of-geometric-progression
  "Port of maths/sum_of_geometric_progression.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare sum-of-geometric-progression)

(defn sum-of-geometric-progression "\"\nReturn the sum of n terms in a geometric progression.\n>>> sum_of_geometric_progression(1, 2, 10)\n1023.0\n>>> sum_of_geometric_progression(1, 10, 5)\n11111.0\n>>> sum_of_geometric_progression(0, 2, 10)\n0.0\n>>> sum_of_geometric_progression(1, 0, 10)\n1.0\n>>> sum_of_geometric_progression(1, 2, 0)\n-0.0\n>>> sum_of_geometric_progression(-1, 2, 10)\n-1023.0\n>>> sum_of_geometric_progression(1, -2, 10)\n-341.0\n>>> sum_of_geometric_progression(1, 2, -10)\n-0.9990234375"
  [first-term common-ratio num-of-terms] (try (when (py-truthy (py-eq common-ratio 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul num-of-terms first-term)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-div first-term (py-sub 1 common-ratio)) (py-sub 1 (py-pow common-ratio num-of-terms)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
