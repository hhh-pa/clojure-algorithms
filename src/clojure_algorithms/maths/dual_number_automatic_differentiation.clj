(ns clojure-algorithms.maths.dual-number-automatic-differentiation
  "Port of maths/dual_number_automatic_differentiation.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare differentiate make-Dual Dual-repr Dual-reduce Dual-add Dual-sub Dual-mul Dual-truediv Dual-floordiv Dual-pow Dual---radd-- Dual---rmul--)

(defrecord Dual [real duals])

(defn make-Dual "Constructor for Dual."
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn Dual-repr
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn Dual-reduce
  [& _args]
  ;; stub: original requires complex-attr,numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr,numpy-attr:copy" {:type :UnsupportedOperation})))

(defn Dual-add
  [& _args]
  ;; stub: original requires complex-attr,numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr,numpy-attr:copy" {:type :UnsupportedOperation})))

(defn Dual-sub
  [self other] (py-add @self (py-mul other (py-neg 1))))

(defn Dual-mul
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn Dual-truediv
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn Dual-floordiv
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn Dual-pow
  [self n] (let [--v (atom nil) x (atom nil)] (try (when (py-truthy (let [__b1 (py-lt n 0)] (if (py-truthy __b1) __b1 (number? n)))) (throw (ex-info (py-str "power must be a positive integer") {:type :ValueError}))) (when (py-truthy (py-eq n 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (when (py-truthy (py-eq n 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @self}))) (reset! x @self) (doseq [__i2 (py-iter (range (py-sub n 1)))] (reset! --v __i2) (swap! x py-mul @self)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @x})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn differentiate ">>> differentiate(lambda x: x**2, 2, 2)\n2\n>>> differentiate(lambda x: x**2 * x**4, 9, 2)\n196830\n>>> differentiate(lambda y: 0.5 * (y + 3) ** 6, 3.5, 4)\n7605.0\n>>> differentiate(lambda y: y ** 2, 4, 3)\n0\n>>> differentiate(8, 8, 8)\nTraceback (most recent call last):\n    ...\nValueError: differentiate() requires a function as input for func\n>>> differentiate(lambda x: x **2, \"\", 1)\nTraceback (most recent call last):\n    ...\nValueError: differentiate() requires a float as input for position\n>>> differentiate(lambda x: x**2, 3, \"\")\nTraceback (most recent call last):\n    ...\nValueError: differentiate() requires an int as input for order"
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(def Dual---radd-- Dual-add)

(def Dual---rmul-- Dual-mul)

(defmethod print-method Dual [o w] (.write w (Dual-repr (atom o))))

(defmethod py-dunder [:add Dual] [_ a b] (Dual-add (atom a) b))

(defmethod py-dunder [:sub Dual] [_ a b] (Dual-sub (atom a) b))

(defmethod py-dunder [:mul Dual] [_ a b] (Dual-mul (atom a) b))

(defmethod py-dunder [:div Dual] [_ a b] (Dual-truediv (atom a) b))

(defmethod py-dunder [:floordiv Dual] [_ a b] (Dual-floordiv (atom a) b))

(defmethod py-dunder [:pow Dual] [_ a b] (Dual-pow (atom a) b))

(defmethod py-dunder [:add-r Dual] [_ a b] (Dual-add (atom b) a))

(defmethod py-dunder [:mul-r Dual] [_ a b] (Dual-mul (atom b) a))

;; NOTE: Dual __init__ stubbed (complex-attr)
;; NOTE: Dual.__repr__ stubbed (complex-attr)
;; NOTE: Dual.reduce stubbed (complex-attr,numpy-attr:copy)
;; NOTE: Dual.__add__ stubbed (complex-attr,numpy-attr:copy)
;; NOTE: Dual.__mul__ stubbed (complex-attr)
;; NOTE: Dual.__truediv__ stubbed (complex-attr)
;; NOTE: Dual.__floordiv__ stubbed (complex-attr)
;; NOTE: differentiate stubbed (complex-attr)
