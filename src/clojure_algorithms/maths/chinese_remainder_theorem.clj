(ns clojure-algorithms.maths.chinese-remainder-theorem
  "Chinese Remainder Theorem:\nGCD ( Greatest Common Divisor ) or HCF ( Highest Common Factor )\n\nIf GCD(a,b) = 1, then for any remainder ra modulo a and any remainder rb modulo b\nthere exists integer n, such that n = ra (mod a) and n = ra(mod b).  If n1 and n2 are\ntwo such integers, then n1=n2(mod ab)\n\nAlgorithm :\n\n1. Use extended euclid algorithm to find x,y such that a*x + b*y = 1\n2. Take n = ra*by + rb*ax"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare extended-euclid chinese-remainder-theorem invert-modulo chinese-remainder-theorem2)

(defn extended-euclid ">>> extended_euclid(10, 6)\n(-1, 2)\n\n>>> extended_euclid(7, 5)\n(-2, 3)"
  [a b] (let [k (atom nil) x (atom nil) y (atom nil)] (try (when (py-truthy (py-eq b 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [1 0]}))) (let [__d1 (vec (extended-euclid b (py-mod a b)))] (reset! x (nth __d1 0)) (reset! y (nth __d1 1))) (reset! k (py-floordiv a b)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@y (py-sub @x (py-mul @k @y))]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn chinese-remainder-theorem ">>> chinese_remainder_theorem(5,1,7,3)\n31\n\nExplanation : 31 is the smallest number such that\n            (i)  When we divide it by 5, we get remainder 1\n            (ii) When we divide it by 7, we get remainder 3\n\n>>> chinese_remainder_theorem(6,1,4,3)\n14"
  [n1 r1 n2 r2] (let [m (atom nil) n (atom nil) x (atom nil) y (atom nil)] (try (let [__d1 (vec (extended-euclid n1 n2))] (reset! x (nth __d1 0)) (reset! y (nth __d1 1))) (reset! m (py-mul n1 n2)) (reset! n (py-add (py-mul (py-mul r2 @x) n1) (py-mul (py-mul r1 @y) n2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-add (py-mod @n @m) @m) @m)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn invert-modulo ">>> invert_modulo(2, 5)\n3\n\n>>> invert_modulo(8,7)\n1"
  [a n] (let [-x (atom nil) b (atom nil)] (try (let [__d1 (vec (extended-euclid a n))] (reset! b (nth __d1 0)) (reset! -x (nth __d1 1))) (when (py-truthy (py-lt @b 0)) (reset! b (py-mod (py-add (py-mod @b n) n) n))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @b})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn chinese-remainder-theorem2 ">>> chinese_remainder_theorem2(5,1,7,3)\n31\n\n>>> chinese_remainder_theorem2(6,1,4,3)\n14"
  [n1 r1 n2 r2] (let [m (atom nil) n (atom nil) x (atom nil) y (atom nil)] (try (let [__d1 (vec [(invert-modulo n1 n2) (invert-modulo n2 n1)])] (reset! x (nth __d1 0)) (reset! y (nth __d1 1))) (reset! m (py-mul n1 n2)) (reset! n (py-add (py-mul (py-mul r2 @x) n1) (py-mul (py-mul r1 @y) n2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-add (py-mod @n @m) @m) @m)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
