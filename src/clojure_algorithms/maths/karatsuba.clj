(ns clojure-algorithms.maths.karatsuba
  "Multiply two numbers using Karatsuba algorithm"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare karatsuba main)

(defn karatsuba ">>> karatsuba(15463, 23489) == 15463 * 23489\nTrue\n>>> karatsuba(3, 9) == 3 * 9\nTrue"
  [a b] (let [a1 (atom nil) a2 (atom nil) b1 (atom nil) b2 (atom nil) m1 (atom nil) m2 (atom nil) x (atom nil) y (atom nil) z (atom nil)] (try (when (py-truthy (let [__b1 (py-eq (count (py-str a)) 1)] (if (py-truthy __b1) __b1 (py-eq (count (py-str b)) 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul a b)}))) (reset! m1 (py-max [(count (py-str a)) (count (py-str b))])) (reset! m2 (py-floordiv @m1 2)) (let [__d2 (vec [(py-floordiv a (py-pow 10 @m2)) (py-mod a (py-pow 10 @m2))])] (reset! a1 (nth __d2 0)) (reset! a2 (nth __d2 1))) (let [__d3 (vec [(py-floordiv b (py-pow 10 @m2)) (py-mod b (py-pow 10 @m2))])] (reset! b1 (nth __d3 0)) (reset! b2 (nth __d3 1))) (reset! x (karatsuba @a2 @b2)) (reset! y (karatsuba (py-add @a1 @a2) (py-add @b1 @b2))) (reset! z (karatsuba @a1 @b1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-add (py-mul @z (py-pow 10 (py-mul 2 @m2))) (py-mul (py-sub (py-sub @y @z) @x) (py-pow 10 @m2))) @x)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [] (println (clojure.string/join " " [(py-str (karatsuba 15463 23489))])) nil)
