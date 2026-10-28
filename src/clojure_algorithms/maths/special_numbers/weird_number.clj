(ns clojure-algorithms.maths.special-numbers.weird-number
  "https://en.wikipedia.org/wiki/Weird_number\n\nFun fact: The set of weird numbers has positive asymptotic density."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare factors abundant semi-perfect weird)

(defn factors ">>> factors(12)\n[1, 2, 3, 4, 6]\n>>> factors(1)\n[1]\n>>> factors(100)\n[1, 2, 4, 5, 10, 20, 25, 50]\n\n# >>> factors(-12)\n# [1, 2, 3, 4, 6]"
  [number] (let [i (atom nil) values (atom nil)] (try (reset! values [1]) (doseq [__i1 (py-iter (range 2 (py-add (py-int (Math/sqrt number) nil) 1) 1))] (reset! i __i1) (when (py-truthy (py-eq (py-mod number @i) 0)) (swap! values conj @i) (when (py-truthy (not (py-eq (py-int (py-floordiv number @i) nil) @i))) (swap! values conj (py-int (py-floordiv number @i) nil))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sorted @values)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn abundant ">>> abundant(0)\nTrue\n>>> abundant(1)\nFalse\n>>> abundant(12)\nTrue\n>>> abundant(13)\nFalse\n>>> abundant(20)\nTrue\n\n# >>> abundant(-12)\n# True"
  [n] (py-gt (reduce +' 0 (factors n)) n))

(defn semi-perfect ">>> semi_perfect(0)\nTrue\n>>> semi_perfect(1)\nTrue\n>>> semi_perfect(12)\nTrue\n>>> semi_perfect(13)\nFalse\n\n# >>> semi_perfect(-12)\n# True"
  [number] (let [i (atom nil) j (atom nil) r (atom nil) subset (atom nil) values (atom nil)] (try (reset! values (factors number)) (reset! r (count @values)) (reset! subset (vec (for [j (py-iter (range (py-add @r 1)))] (vec (for [i (py-iter (range (py-add number 1)))] 0))))) (doseq [__i1 (py-iter (range (py-add @r 1)))] (reset! i __i1) (swap! subset (fn [__c] (py-assoc-in __c [@i 0] true)))) (doseq [__i2 (py-iter (range 1 (py-add number 1)))] (reset! i __i2) (swap! subset (fn [__c] (py-assoc-in __c [0 @i] false)))) (doseq [__i3 (py-iter (range 1 (py-add @r 1)))] (reset! i __i3) (doseq [__i4 (py-iter (range 1 (py-add number 1)))] (reset! j __i4) (if (py-truthy (py-lt @j (py-get @values (py-sub @i 1)))) (do (swap! subset (fn [__c] (py-assoc-in __c [@i @j] (py-get (py-get @subset (py-sub @i 1)) @j))))) (do (swap! subset (fn [__c] (py-assoc-in __c [@i @j] (let [__b5 (py-get (py-get @subset (py-sub @i 1)) @j)] (if (py-truthy __b5) __b5 (py-get (py-get @subset (py-sub @i 1)) (py-sub @j (py-get @values (py-sub @i 1))))))))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (not (py-eq (py-get (py-get @subset @r) number) 0))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn weird ">>> weird(0)\nFalse\n>>> weird(70)\nTrue\n>>> weird(77)\nFalse"
  [number] (let [__b1 (abundant number)] (if (py-truthy __b1) (not (py-truthy (semi-perfect number))) __b1)))
