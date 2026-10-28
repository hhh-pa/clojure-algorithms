(ns clojure-algorithms.maths.disarium
  "Port of maths/disarium.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare is-disarium)

(defn is-disarium "Check if a number is Disarium.\n\nhttps://rosettacode.org/wiki/Disarium_numbers\n\n>>> is_disarium(89)\nTrue\n>>> is_disarium(75)\nFalse\n>>> is_disarium(135)\nTrue"
  [num] (let [digits (atom nil) i (atom nil) total (atom nil)] (try (reset! digits (py-list (py-str num))) (reset! total 0) (doseq [__i1 (py-iter (range (count @digits)))] (reset! i __i1) (swap! total py-add (py-pow (py-int (py-get @digits @i) nil) (py-add @i 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @total num)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
