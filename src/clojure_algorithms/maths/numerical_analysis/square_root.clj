(ns clojure-algorithms.maths.numerical-analysis.square-root
  "Port of maths/numerical_analysis/square_root.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare fx fx-derivative get-initial-point square-root-iterative)

(defn fx
  [x a] (py-sub (py-pow x 2) a))

(defn fx-derivative
  [x] (py-mul 2 x))

(defn get-initial-point
  [a] (let [start (atom nil)] (try (reset! start 2.0) (while (py-truthy (py-le @start a)) (reset! start (py-pow @start 2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @start})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn square-root-iterative "Square root approximated using Newton's method.\nhttps://en.wikipedia.org/wiki/Newton%27s_method\n\n>>> all(abs(square_root_iterative(i) - math.sqrt(i)) <= 1e-14 for i in range(500))\nTrue\n\n>>> square_root_iterative(-1)\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n\n>>> square_root_iterative(4)\n2.0\n\n>>> square_root_iterative(3.2)\n1.788854381999832\n\n>>> square_root_iterative(140)\n11.832159566199232"
  ([a] (square-root-iterative a 9999 1e-14))
  ([a max-iter] (square-root-iterative a max-iter 1e-14))
  ([a max-iter tolerance] (let [--v (atom nil) prev-value (atom nil) value (atom nil)] (try (when (py-truthy (py-lt a 0)) (throw (ex-info (py-str "math domain error") {:type :ValueError}))) (reset! value (get-initial-point a)) (doseq [__i1 (py-iter (range max-iter))] (reset! --v __i1) (reset! prev-value @value) (reset! value (py-sub @value (py-div (fx @value a) (fx-derivative @value)))) (when (py-truthy (py-lt (py-abs (py-sub @prev-value @value)) tolerance)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @value})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
