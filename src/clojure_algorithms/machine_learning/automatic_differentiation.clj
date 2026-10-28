(ns clojure-algorithms.machine-learning.automatic-differentiation
  "Demonstration of the Automatic Differentiation (Reverse mode).\n\nReference: https://en.wikipedia.org/wiki/Automatic_differentiation\n\nAuthor: Poojan Smart\nEmail: smrtpoojan@gmail.com"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare OpType OpType-ADD OpType-SUB OpType-MUL OpType-DIV OpType-MATMUL OpType-POWER OpType-NOOP make-OpType make-Variable Variable-repr Variable-to-ndarray Variable-add Variable-sub Variable-mul Variable-truediv Variable-matmul Variable-pow Variable-add-param-to Variable-add-result-of make-Operation Operation-add-params Operation-add-output Operation-eq make-GradientTracker GradientTracker-enter GradientTracker-exit GradientTracker-append GradientTracker-gradient GradientTracker-derivative GradientTracker-instance)

(defrecord Variable [value param-to result-of])

(defrecord Operation [op-type other-params params output])

(defrecord GradientTracker [enabled instance])

(defn make-OpType [v] (get (into {} (map (fn [[k w]] [w k])) OpType) v v))

(defn make-Variable "Constructor for Variable. Class represents n-dimensional object which is used to wrap numpy array on which\noperations will be performed and the gradient will be calculated.\n\nExamples:\n>>> Variable(5.0)\nVariable(5.0)\n>>> Variable([5.0, 2.9])\nVariable([5.  2.9])\n>>> Variable([5.0, 2.9]) + Variable([1.0, 5.5])\nVariable([6.  8.4])\n>>> Variable([[8.0, 10.0]])\nVariable([[ 8. 10.]])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn make-Operation "Constructor for Operation. Class represents operation between single or two Variable objects.\nOperation objects contains type of operation, pointers to input Variable\nobjects and pointer to resulting Variable from the operation."
  ([op-type] (make-Operation op-type nil))
  ([op-type other-params] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:op-type] op-type))) (swap! self (fn [__c] (py-assoc-in __c [:other-params] (if (py-truthy (nil? other-params)) {} other-params)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Operation @self))))

(defn make-GradientTracker "Constructor for GradientTracker. Class contains methods to compute partial derivatives of Variable\nbased on the computation graph.\n\nExamples:\n\n>>> with GradientTracker() as tracker:\n...     a = Variable([2.0, 5.0])\n...     b = Variable([1.0, 2.0])\n...     m = Variable([1.0, 2.0])\n...     c = a + b\n...     d = a * b\n...     e = c / d\n>>> tracker.gradient(e, a)\narray([-0.25, -0.04])\n>>> tracker.gradient(e, b)\narray([-1.  , -0.25])\n>>> tracker.gradient(e, m) is None\nTrue\n\n>>> with GradientTracker() as tracker:\n...     a = Variable([[2.0, 5.0]])\n...     b = Variable([[1.0], [2.0]])\n...     c = a @ b\n>>> tracker.gradient(c, a)\narray([[1., 2.]])\n>>> tracker.gradient(c, b)\narray([[2.],\n       [5.]])\n\n>>> with GradientTracker() as tracker:\n...     a = Variable([[2.0, 5.0]])\n...     b = a ** 3\n>>> tracker.gradient(b, a)\narray([[12., 75.]])"
  [] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:enabled] false))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->GradientTracker @self)))

(defn Variable-repr
  [self] (str "Variable(" (py-str (:value @self)) ")"))

(defn Variable-to-ndarray
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Variable-add
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-add (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-ADD)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-sub
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-sub (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-SUB)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-mul
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-mul (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-MUL)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-truediv
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-div (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-DIV)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-matmul
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-matmul (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-MATMUL)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-pow
  [self power] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-pow (:value @self) power))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-POWER)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-add-param-to
  [self param-to] (conj (vec (:param-to @self)) param-to) nil)

(defn Variable-add-result-of
  [self result-of] (swap! self (fn [__c] (py-assoc-in __c [:result-of] result-of))) nil)

(defn Operation-add-params
  [self params] (swap! self (fn [__c] (py-assoc-in __c [:params] params))) nil)

(defn Operation-add-output
  [self output] (swap! self (fn [__c] (py-assoc-in __c [:output] output))) nil)

(defn Operation-eq
  [self value] (if (py-truthy (contains? (set (vals OpType)) value)) (py-eq (:op-type @self) value) false))

(defn GradientTracker-enter
  [self] (try (swap! self (fn [__c] (py-assoc-in __c [:enabled] true))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @self})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn GradientTracker-exit
  [self exc-type exc traceback] (swap! self (fn [__c] (py-assoc-in __c [:enabled] false))) nil)

(defn GradientTracker-append "Adds Operation object to the related Variable objects for\ncreating computational graph for calculating gradients.\n\nArgs:\n    op_type: Operation type\n    params: Input parameters to the operation\n    output: Output variable of the operation"
  ([self op-type params output] (GradientTracker-append self op-type params output nil))
  ([self op-type params output other-params] (let [operation (atom nil) param (atom nil) param-nodes (atom nil)] (reset! operation (make-Operation op-type nil)) (reset! param-nodes []) (doseq [__i1 (py-iter params)] (reset! param __i1) (Variable-add-param-to param @operation) (swap! param-nodes conj @param)) (Variable-add-result-of (atom output) @operation) (Operation-add-params operation @param-nodes) (Operation-add-output operation output) nil)))

(defn GradientTracker-gradient "Reverse accumulation of partial derivatives to calculate gradients\nof target variable with respect to source variable.\n\nArgs:\n    target: target variable for which gradients are calculated.\n    source: source variable with respect to which the gradients are\n    calculated.\n\nReturns:\n    Gradient of the source variable with respect to the target variable"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn GradientTracker-derivative "Compute the derivative of given operation/function\n\nArgs:\n    param: variable to be differentiated\n    operation: function performed on the input variable\n\nReturns:\n    Derivative of input variable with respect to the output of\n    the operation"
  [& _args]
  ;; stub: original requires np,numpy-attr:T; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T" {:type :UnsupportedOperation})))

(def OpType
  "Class represents list of supported operations on Variable for gradient calculation."
  {:ADD 0 :SUB 1 :MUL 2 :DIV 3 :MATMUL 4 :POWER 5 :NOOP 6})

(def OpType-ADD 0)

(def OpType-SUB 1)

(def OpType-MUL 2)

(def OpType-DIV 3)

(def OpType-MATMUL 4)

(def OpType-POWER 5)

(def OpType-NOOP 6)

(def GradientTracker-instance nil)

(defmethod print-method Variable [o w] (.write w (Variable-repr (atom o))))

(defmethod py-dunder [:add Variable] [_ a b] (Variable-add (atom a) b))

(defmethod py-dunder [:sub Variable] [_ a b] (Variable-sub (atom a) b))

(defmethod py-dunder [:mul Variable] [_ a b] (Variable-mul (atom a) b))

(defmethod py-dunder [:div Variable] [_ a b] (Variable-truediv (atom a) b))

(defmethod py-dunder [:matmul Variable] [_ a b] (Variable-matmul (atom a) b))

(defmethod py-dunder [:pow Variable] [_ a b] (Variable-pow (atom a) b))

(defmethod py-dunder [:eq Operation] [_ a b] (Operation-eq (atom a) b))

;; NOTE: Variable __init__ stubbed (np)
;; NOTE: Variable.to_ndarray stubbed (np)
;; NOTE: GradientTracker.gradient stubbed (np)
;; NOTE: GradientTracker.derivative stubbed (np,numpy-attr:T)
