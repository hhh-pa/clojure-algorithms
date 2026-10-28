(ns clojure-algorithms.maths.numerical-analysis.secant-method
  "Implementing Secant method in Python\nAuthor: dimgrichr"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare f secant-method)

(defn f ">>> f(5)\n39.98652410600183"
  [x] (py-sub (py-mul 8 x) (py-mul 2 (Math/exp (py-neg x)))))

(defn secant-method ">>> secant_method(1, 3, 2)\n0.2139409276214589"
  [lower-bound upper-bound repeats] (let [--v (atom nil) x0 (atom nil) x1 (atom nil)] (try (reset! x0 lower-bound) (reset! x1 upper-bound) (doseq [__i1 (py-iter (range repeats))] (reset! --v __i1) (let [__d2 (vec [@x1 (py-sub @x1 (py-div (py-mul (f @x1) (py-sub @x1 @x0)) (py-sub (f @x1) (f @x0))))])] (reset! x0 (nth __d2 0)) (reset! x1 (nth __d2 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @x1})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
