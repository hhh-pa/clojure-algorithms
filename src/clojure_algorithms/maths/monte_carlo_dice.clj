(ns clojure-algorithms.maths.monte-carlo-dice
  "Port of maths/monte_carlo_dice.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare throw-dice make-Dice Dice-roll Dice-NUM-SIDES)

(defrecord Dice [sides])

(defn make-Dice "Constructor for Dice."
  [] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:sides] (py-list (range 1 (py-add Dice-NUM-SIDES 1)))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Dice @self)))

(defn Dice-roll
  [self] (rand-nth (vec (:sides @self))))

(defn throw-dice "Return probability list of all possible sums when throwing dice.\n\n>>> random.seed(0)\n>>> throw_dice(10, 1)\n[10.0, 0.0, 30.0, 50.0, 10.0, 0.0]\n>>> throw_dice(100, 1)\n[19.0, 17.0, 17.0, 11.0, 23.0, 13.0]\n>>> throw_dice(1000, 1)\n[18.8, 15.5, 16.3, 17.6, 14.2, 17.6]\n>>> throw_dice(10000, 1)\n[16.35, 16.89, 16.93, 16.6, 16.52, 16.71]\n>>> throw_dice(10000, 2)\n[2.74, 5.6, 7.99, 11.26, 13.92, 16.7, 14.44, 10.63, 8.05, 5.92, 2.75]"
  ([num-throws] (throw-dice num-throws 2))
  ([num-throws num-dice] (let [--v (atom nil) count-of-sum (atom nil) dices (atom nil) probability (atom nil)] (try (reset! dices (vec (for [i (py-iter (range num-dice))] (make-Dice)))) (reset! count-of-sum (py-mul [0] (py-add (py-mul (count @dices) Dice-NUM-SIDES) 1))) (doseq [__i1 (py-iter (range num-throws))] (reset! --v __i1) (swap! count-of-sum (fn [__c] (py-assoc-in __c [(reduce +' 0 (doall (for [dice (py-iter @dices)] (Dice-roll (atom dice)))))] (py-add (py-get-in __c [(reduce +' 0 (doall (for [dice (py-iter @dices)] (Dice-roll (atom dice)))))]) 1))))) (reset! probability (vec (for [count-v (py-iter @count-of-sum)] (py-round (py-div (py-mul count-v 100) num-throws) 2)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-slice @probability num-dice nil nil)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(def Dice-NUM-SIDES 6)
