(ns clojure-algorithms.maths.modular-exponential
  "Modular Exponential.\nModular exponentiation is a type of exponentiation performed over a modulus.\nFor more explanation, please check\nhttps://en.wikipedia.org/wiki/Modular_exponentiation"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare modular-exponential main)

(defn modular-exponential ">>> modular_exponential(5, 0, 10)\n1\n>>> modular_exponential(2, 8, 7)\n4\n>>> modular_exponential(3, -2, 9)\n-1"
  [base power mod-v] (let [base (atom base) power (atom power)] (let [result (atom nil)] (try (when (py-truthy (py-lt @power 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-neg 1)}))) (swap! base py-mod mod-v) (reset! result 1) (while (py-truthy (py-gt @power 0)) (when (py-truthy (py-band @power 1)) (reset! result (py-mod (py-mul @result @base) mod-v))) (reset! power (.shiftRight (.toBigInteger (bigint @power)) (int 1))) (reset! base (py-mod (py-mul @base @base) mod-v))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn main "Call Modular Exponential Function."
  [] (println (clojure.string/join " " [(py-str (modular-exponential 3 200 13))])) nil)
