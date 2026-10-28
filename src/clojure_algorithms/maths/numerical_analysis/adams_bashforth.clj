(ns clojure-algorithms.maths.numerical-analysis.adams-bashforth
  "Use the Adams-Bashforth methods to solve Ordinary Differential Equations.\n\nhttps://en.wikipedia.org/wiki/Linear_multistep_method\nAuthor : Ravi Kumar"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare make-AdamsBashforth AdamsBashforth-post-init AdamsBashforth-step-2 AdamsBashforth-step-3 AdamsBashforth-step-4 AdamsBashforth-step-5)

(defrecord AdamsBashforth [func x-initials y-initials step-size x-final])

(defn make-AdamsBashforth "Constructor for AdamsBashforth. args:\nfunc: An ordinary differential equation (ODE) as function of x and y.\nx_initials: List containing initial required values of x.\ny_initials: List containing initial required values of y.\nstep_size: The increment value of x.\nx_final: The final value of x.\n\nReturns: Solution of y at each nodal point\n\n>>> def f(x, y):\n...     return x + y\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0.2, 1], 0.2, 1)  # doctest: +ELLIPSIS\nAdamsBashforth(func=..., x_initials=[0, 0.2, 0.4], y_initials=[0, 0.2, 1], step...)\n>>> AdamsBashforth(f, [0, 0.2, 1], [0, 0, 0.04], 0.2, 1).step_2()\nTraceback (most recent call last):\n    ...\nValueError: The final value of x must be greater than the initial values of x.\n\n>>> AdamsBashforth(f, [0, 0.2, 0.3], [0, 0, 0.04], 0.2, 1).step_3()\nTraceback (most recent call last):\n    ...\nValueError: x-values must be equally spaced according to step size.\n\n>>> AdamsBashforth(f,[0,0.2,0.4,0.6,0.8],[0,0,0.04,0.128,0.307],-0.2,1).step_5()\nTraceback (most recent call last):\n    ...\nValueError: Step size must be positive."
  [func x-initials y-initials step-size x-final] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:func] func))) (swap! self (fn [__c] (py-assoc-in __c [:x-initials] x-initials))) (swap! self (fn [__c] (py-assoc-in __c [:y-initials] y-initials))) (swap! self (fn [__c] (py-assoc-in __c [:step-size] step-size))) (swap! self (fn [__c] (py-assoc-in __c [:x-final] x-final))) (AdamsBashforth-post-init self) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->AdamsBashforth @self)))

(defn AdamsBashforth-post-init
  [self] (when (py-truthy (py-ge (py-get (:x-initials @self) (py-neg 1)) (:x-final @self))) (throw (ex-info (py-str "The final value of x must be greater than the initial values of x.") {:type :ValueError}))) (when (py-truthy (py-le (:step-size @self) 0)) (throw (ex-info (py-str "Step size must be positive.") {:type :ValueError}))) (when (py-truthy (not (py-truthy (every? py-truthy (doall (for [[x0 x1] (py-iter (map vector (:x-initials @self) (py-slice (:x-initials @self) 1 nil nil)))] (py-eq (py-round (py-sub x1 x0) 10) (:step-size @self)))))))) (throw (ex-info (py-str "x-values must be equally spaced according to step size.") {:type :ValueError}))) nil)

(defn AdamsBashforth-step-2 ">>> def f(x, y):\n...     return x\n>>> AdamsBashforth(f, [0, 0.2], [0, 0], 0.2, 1).step_2()\narray([0.  , 0.  , 0.06, 0.16, 0.3 , 0.48])\n\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_2()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn AdamsBashforth-step-3 ">>> def f(x, y):\n...     return x + y\n>>> y = AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_3()\n>>> float(y[3])\n0.15533333333333332\n\n>>> AdamsBashforth(f, [0, 0.2], [0, 0], 0.2, 1).step_3()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn AdamsBashforth-step-4 ">>> def f(x,y):\n...     return x + y\n>>> y = AdamsBashforth(\n...    f, [0, 0.2, 0.4, 0.6], [0, 0, 0.04, 0.128], 0.2, 1).step_4()\n>>> float(y[4])\n0.30699999999999994\n>>> float(y[5])\n0.5771083333333333\n\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_4()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn AdamsBashforth-step-5 ">>> def f(x,y):\n...     return x + y\n>>> y = AdamsBashforth(\n...     f, [0, 0.2, 0.4, 0.6, 0.8], [0, 0.02140, 0.02140, 0.22211, 0.42536],\n...     0.2, 1).step_5()\n>>> float(y[-1])\n0.05436839444444452\n\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_5()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: AdamsBashforth.step_2 stubbed (np)
;; NOTE: AdamsBashforth.step_3 stubbed (np)
;; NOTE: AdamsBashforth.step_4 stubbed (np)
;; NOTE: AdamsBashforth.step_5 stubbed (np)
