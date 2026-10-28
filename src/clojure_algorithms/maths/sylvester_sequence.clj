(ns clojure-algorithms.maths.sylvester-sequence
  "Calculates the nth number in Sylvester's sequence\n\nSource:\n    https://en.wikipedia.org/wiki/Sylvester%27s_sequence"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare sylvester)

(defn sylvester ":param number: nth number to calculate in the sequence\n:return: the nth number in Sylvester's sequence\n\n>>> sylvester(8)\n113423713055421844361000443\n\n>>> sylvester(-1)\nTraceback (most recent call last):\n    ...\nValueError: The input value of [n=-1] has to be > 0\n\n>>> sylvester(8.0)\nTraceback (most recent call last):\n    ...\nAssertionError: The input value of [n=8.0] is not an integer"
  [number] (let [lower (atom nil) msg (atom nil) num (atom nil) upper (atom nil)] (try (assert (py-truthy (integer? number)) (py-str (str "The input value of [n=" (py-str number) "] is not an integer"))) (if (py-truthy (py-eq number 1)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value 2}))) (do (if (py-truthy (py-lt number 1)) (do (reset! msg (str "The input value of [n=" (py-str number) "] has to be > 0")) (throw (ex-info (py-str @msg) {:type :ValueError}))) (do (reset! num (sylvester (py-sub number 1))) (reset! lower (py-sub @num 1)) (reset! upper @num) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-mul @lower @upper) 1)})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
