(ns clojure-algorithms.maths.convolve-1d
  "Port of maths/convolve_1d.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare make-Signal Signal-post-init make-DiscreteConvolve1D DiscreteConvolve1D-convolve-1d)

(defrecord Signal [signal n])

(defrecord DiscreteConvolve1D [kern sig])

(defn make-Signal "Constructor for Signal. A discrete representation of a signal as a n-dimensional vector\n\n>>> Signal([1.0,3.0,2.0,-1.0])\nSignal(signal=[1.0, 3.0, 2.0, -1.0], n=4)"
  ([] (make-Signal (py-list []) 0))
  ([signal] (make-Signal signal 0))
  ([signal n] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:signal] signal))) (swap! self (fn [__c] (py-assoc-in __c [:n] n))) (Signal-post-init self) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Signal @self))))

(defn make-DiscreteConvolve1D "Constructor for DiscreteConvolve1D. 1D discrete convolution between two linear signals\n\n>>> s1 = Signal([1,2,3,4,5])\n>>> s2 = Signal([1,-1,2,-3])\n>>> DiscreteConvolve1D(s1,s2) # doctest: +NORMALIZE_WHITESPACE\nDiscreteConvolve1D(kern=Signal(signal=[1, 2, 3, 4, 5], n=5),\nsig=Signal(signal=[1, -1, 2, -3], n=4))"
  ([] (make-DiscreteConvolve1D (make-Signal) (make-Signal)))
  ([kern] (make-DiscreteConvolve1D kern (make-Signal)))
  ([kern sig] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:kern] kern))) (swap! self (fn [__c] (py-assoc-in __c [:sig] sig))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->DiscreteConvolve1D @self))))

(defn Signal-post-init
  [self] (let [i (atom nil)] (doseq [__i1 (py-iter (:signal @self))] (reset! i __i1) (when (py-truthy (not (py-truthy (or (number? @i) (integer? @i))))) (throw (ex-info (py-str "vector must be a list of numeric values.") {:type :TypeError}))) (swap! self (fn [__c] (py-assoc-in __c [:n] (py-add (py-get-in __c [:n]) 1))))) nil))

(defn DiscreteConvolve1D-convolve-1d
  [self] (let [conv (atom nil) i (atom nil) j (atom nil) sig-val (atom nil)] (try (reset! conv (make-Signal)) (doseq [__i1 (py-iter (range (:n (:sig @self))))] (reset! i __i1) (conj (vec (:signal @conv)) 0) (doseq [__i2 (py-iter (range (:n (:kern @self))))] (reset! j __i2) (if (py-truthy (let [__b3 (py-lt (py-sub (py-add @i @j) (py-floor (py-div (:n (:kern @self)) 2))) 0)] (if (py-truthy __b3) __b3 (py-ge (py-sub (py-add @i @j) (py-floor (py-div (:n (:kern @self)) 2))) (:n (:sig @self)))))) (do (reset! sig-val 0.0)) (do (reset! sig-val (py-float (py-get (:signal (:sig @self)) (py-sub (py-add @i @j) (py-floor (py-div (:n (:kern @self)) 2)))))))) (swap! conv (fn [__c] (py-assoc-in __c [:signal @i] (py-add (py-get-in __c [:signal @i]) (py-mul (py-get (:signal (:kern @self)) @j) @sig-val)))))) (swap! conv (fn [__c] (py-assoc-in __c [:n] (py-add (py-get-in __c [:n]) 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @conv})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
