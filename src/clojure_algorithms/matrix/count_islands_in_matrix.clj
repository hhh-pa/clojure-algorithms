(ns clojure-algorithms.matrix.count-islands-in-matrix
  "Port of matrix/count_islands_in_matrix.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare make-Matrix Matrix-is-safe Matrix-diffs Matrix-count-islands)

(defrecord Matrix [graph ROW COL])

(defn make-Matrix "Constructor for Matrix."
  [graph] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:graph] graph))) (swap! self (fn [__c] (py-assoc-in __c [:ROW] (count graph)))) (swap! self (fn [__c] (py-assoc-in __c [:COL] (count (py-get graph 0))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Matrix @self)))

(defn Matrix-is-safe ">>> visited = [[False, False, False],\n...             [False, False, False],\n...             [False, False, False]]\n>>> m = Matrix([[True, False, False],\n...              [False, False, True],\n...              [False, False, True]])\n>>> m.is_safe(0, 0, visited)\nTrue\n>>> m.is_safe(0, 2, visited)\nFalse\n>>> m.is_safe(-1, 2, visited)\nFalse\n>>> m.is_safe(1, 5, visited)\nFalse"
  [self i j visited] (let [__b9 (let [__c1 0 __c2 i __c3 (:ROW @self)] (and (py-le __c1 __c2) (py-lt __c2 __c3)))] (if (py-truthy __b9) (let [__b8 (let [__c4 0 __c5 j __c6 (:COL @self)] (and (py-le __c4 __c5) (py-lt __c5 __c6)))] (if (py-truthy __b8) (let [__b7 (not (py-truthy (py-get (py-get visited i) j)))] (if (py-truthy __b7) (py-eq (py-get (py-get (:graph @self) i) j) 1) __b7)) __b8)) __b9)))

(defn Matrix-diffs "Checking all 8 elements surrounding nth element.\n\n>>> visited = [[False, False, False],\n...             [False, False, False],\n...             [False, False, False]]\n>>> m = Matrix([[True, True, False],\n...              [False, True, False],\n...              [True, False, True]])\n>>> m.diffs(0, 0, visited)\n>>> visited  # doctest: +NORMALIZE_WHITESPACE\n[[True, True, False],\n [False, True, False],\n [True, False, True]]"
  [self i j visited] (let [visited (atom visited)] (let [col-nbr (atom nil) k (atom nil) row-nbr (atom nil)] (reset! row-nbr [(py-neg 1) (py-neg 1) (py-neg 1) 0 0 1 1 1]) (reset! col-nbr [(py-neg 1) 0 1 (py-neg 1) 1 (py-neg 1) 0 1]) (swap! visited (fn [__c] (py-assoc-in __c [i j] true))) (doseq [__i1 (py-iter (range 8))] (reset! k __i1) (when (py-truthy (Matrix-is-safe self (py-add i (py-get @row-nbr @k)) (py-add j (py-get @col-nbr @k)) @visited)) (Matrix-diffs self (py-add i (py-get @row-nbr @k)) (py-add j (py-get @col-nbr @k)) @visited))) nil)))

(defn Matrix-count-islands ">>> m = Matrix([[True, True, False, False],\n...              [False, True, False, True],\n...              [True, False, False, True]])\n>>> m.count_islands()\n2\n>>> m2 = Matrix([[True, True, False],\n...               [True, False, False],\n...               [False, False, True]])\n>>> m2.count_islands()\n2"
  [self] (let [count-v (atom nil) i (atom nil) j (atom nil) visited (atom nil)] (try (reset! visited (vec (for [i (py-iter (range (:ROW @self)))] (vec (for [j (py-iter (range (:COL @self)))] false))))) (reset! count-v 0) (doseq [__i1 (py-iter (range (:ROW @self)))] (reset! i __i1) (doseq [__i2 (py-iter (range (:COL @self)))] (reset! j __i2) (when (py-truthy (let [__b3 (not (py-truthy (py-get (py-get @visited @i) @j)))] (if (py-truthy __b3) (py-get (py-get (:graph @self) @i) @j) __b3))) (Matrix-diffs self @i @j @visited) (swap! count-v py-add 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @count-v})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
