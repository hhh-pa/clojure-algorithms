(ns clojure-algorithms.maths.polynomials.single-indeterminate-operations
  "This module implements a single indeterminate polynomials class\nwith some basic operations\n\nReference: https://en.wikipedia.org/wiki/Polynomial"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare make-Polynomial Polynomial-add Polynomial-sub Polynomial-neg Polynomial-mul Polynomial-evaluate Polynomial-str Polynomial-repr Polynomial-derivative Polynomial-integral Polynomial-eq Polynomial-ne)

(defrecord Polynomial [coefficients degree])

(defn make-Polynomial "Constructor for Polynomial."
  [degree coefficients] (let [self (atom {})] (try (when (py-truthy (not (py-eq (count coefficients) (py-add degree 1)))) (throw (ex-info (py-str "The number of coefficients should be equal to the degree + 1.") {:type :ValueError}))) (swap! self (fn [__c] (py-assoc-in __c [:coefficients] (py-list coefficients)))) (swap! self (fn [__c] (py-assoc-in __c [:degree] degree))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Polynomial @self)))

(defn Polynomial-add "Polynomial addition\n>>> p = Polynomial(2, [1, 2, 3])\n>>> q = Polynomial(2, [1, 2, 3])\n>>> p + q\n6x^2 + 4x + 2"
  [self polynomial-2] (let [coefficients (atom nil) i (atom nil)] (try (if (py-truthy (py-gt (:degree @self) (:degree polynomial-2))) (do (reset! coefficients (py-slice (:coefficients @self) nil nil nil)) (doseq [__i1 (py-iter (range (py-add (:degree polynomial-2) 1)))] (reset! i __i1) (swap! coefficients (fn [__c] (py-assoc-in __c [@i] (py-add (py-get-in __c [@i]) (py-get (:coefficients polynomial-2) @i)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Polynomial (:degree @self) @coefficients)}))) (do (reset! coefficients (py-slice (:coefficients polynomial-2) nil nil nil)) (doseq [__i2 (py-iter (range (py-add (:degree @self) 1)))] (reset! i __i2) (swap! coefficients (fn [__c] (py-assoc-in __c [@i] (py-add (py-get-in __c [@i]) (py-get (:coefficients @self) @i)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Polynomial (:degree polynomial-2) @coefficients)})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Polynomial-sub "Polynomial subtraction\n>>> p = Polynomial(2, [1, 2, 4])\n>>> q = Polynomial(2, [1, 2, 3])\n>>> p - q\n1x^2"
  [self polynomial-2] (py-add @self (py-mul polynomial-2 (make-Polynomial 0 [(py-neg 1)]))))

(defn Polynomial-neg "Polynomial negation\n>>> p = Polynomial(2, [1, 2, 3])\n>>> -p\n - 3x^2 - 2x - 1"
  [self] (make-Polynomial (:degree @self) (vec (for [c (py-iter (:coefficients @self))] (py-neg c)))))

(defn Polynomial-mul "Polynomial multiplication\n>>> p = Polynomial(2, [1, 2, 3])\n>>> q = Polynomial(2, [1, 2, 3])\n>>> p * q\n9x^4 + 12x^3 + 10x^2 + 4x + 1"
  [self polynomial-2] (let [coefficients (atom nil) i (atom nil) j (atom nil)] (try (reset! coefficients (py-mul [0] (py-add (py-add (:degree @self) (:degree polynomial-2)) 1))) (doseq [__i1 (py-iter (range (py-add (:degree @self) 1)))] (reset! i __i1) (doseq [__i2 (py-iter (range (py-add (:degree polynomial-2) 1)))] (reset! j __i2) (swap! coefficients (fn [__c] (py-assoc-in __c [(py-add @i @j)] (py-add (py-get-in __c [(py-add @i @j)]) (py-mul (py-get (:coefficients @self) @i) (py-get (:coefficients polynomial-2) @j)))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Polynomial (py-add (:degree @self) (:degree polynomial-2)) @coefficients)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Polynomial-evaluate "Evaluates the polynomial at x.\n>>> p = Polynomial(2, [1, 2, 3])\n>>> p.evaluate(2)\n17"
  [self substitution] (let [i (atom nil) result (atom nil)] (try (reset! result 0) (doseq [__i1 (py-iter (range (py-add (:degree @self) 1)))] (reset! i __i1) (swap! result py-add (py-mul (py-get (:coefficients @self) @i) (py-pow substitution @i)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Polynomial-str ">>> p = Polynomial(2, [1, 2, 3])\n>>> print(p)\n3x^2 + 2x + 1"
  [self] (let [i (atom nil) polynomial (atom nil)] (try (reset! polynomial "") (doseq [__i1 (py-iter (range (:degree @self) (py-neg 1) (py-neg 1)))] (try (reset! i __i1) (when (py-truthy (py-eq (py-get (:coefficients @self) @i) 0)) (throw (ex-info "__continue__" {:__control__ true :__continue__ true}))) (if (py-truthy (py-gt (py-get (:coefficients @self) @i) 0)) (do (when (py-truthy @polynomial) (swap! polynomial py-add " + "))) (do (swap! polynomial py-add " - "))) (if (py-truthy (py-eq @i 0)) (do (swap! polynomial py-add (py-str (py-abs (py-get (:coefficients @self) @i))))) (do (if (py-truthy (py-eq @i 1)) (do (swap! polynomial py-add (py-add (py-str (py-abs (py-get (:coefficients @self) @i))) "x"))) (do (swap! polynomial py-add (py-add (py-add (py-str (py-abs (py-get (:coefficients @self) @i))) "x^") (py-str @i))))))) (catch clojure.lang.ExceptionInfo __e (if (:__continue__ (ex-data __e)) nil (throw __e))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @polynomial})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Polynomial-repr ">>> p = Polynomial(2, [1, 2, 3])\n>>> p\n3x^2 + 2x + 1"
  [self] (Polynomial-str self))

(defn Polynomial-derivative "Returns the derivative of the polynomial.\n>>> p = Polynomial(2, [1, 2, 3])\n>>> p.derivative()\n6x + 2"
  [self] (let [coefficients (atom nil) i (atom nil)] (try (reset! coefficients (py-mul [0] (:degree @self))) (doseq [__i1 (py-iter (range (:degree @self)))] (reset! i __i1) (swap! coefficients (fn [__c] (py-assoc-in __c [@i] (py-mul (py-get (:coefficients @self) (py-add @i 1)) (py-add @i 1)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Polynomial (py-sub (:degree @self) 1) @coefficients)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Polynomial-integral "Returns the integral of the polynomial.\n>>> p = Polynomial(2, [1, 2, 3])\n>>> p.integral()\n1.0x^3 + 1.0x^2 + 1.0x"
  ([self] (Polynomial-integral self 0))
  ([self constant] (let [coefficients (atom nil) i (atom nil)] (try (reset! coefficients (py-mul [0] (py-add (:degree @self) 2))) (swap! coefficients (fn [__c] (py-assoc-in __c [0] constant))) (doseq [__i1 (py-iter (range (py-add (:degree @self) 1)))] (reset! i __i1) (swap! coefficients (fn [__c] (py-assoc-in __c [(py-add @i 1)] (py-div (py-get (:coefficients @self) @i) (py-add @i 1)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Polynomial (py-add (:degree @self) 1) @coefficients)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn Polynomial-eq "Checks if two polynomials are equal.\n>>> p = Polynomial(2, [1, 2, 3])\n>>> q = Polynomial(2, [1, 2, 3])\n>>> p == q\nTrue"
  [self polynomial-2] (let [i (atom nil)] (try (when (py-truthy (not (py-truthy (instance? Polynomial polynomial-2)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (when (py-truthy (not (py-eq (:degree @self) (:degree polynomial-2)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (doseq [__i1 (py-iter (range (py-add (:degree @self) 1)))] (reset! i __i1) (when (py-truthy (not (py-eq (py-get (:coefficients @self) @i) (py-get (:coefficients polynomial-2) @i)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Polynomial-ne "Checks if two polynomials are not equal.\n>>> p = Polynomial(2, [1, 2, 3])\n>>> q = Polynomial(2, [1, 2, 3])\n>>> p != q\nFalse"
  [self polynomial-2] (not (py-truthy (Polynomial-eq self polynomial-2))))

(defmethod print-method Polynomial [o w] (.write w (if *print-readably* (Polynomial-repr (atom o)) (Polynomial-str (atom o)))))

(defmethod py-dunder [:add Polynomial] [_ a b] (Polynomial-add (atom a) b))

(defmethod py-dunder [:sub Polynomial] [_ a b] (Polynomial-sub (atom a) b))

(defmethod py-dunder [:neg Polynomial] [_ a] (Polynomial-neg (atom a)))

(defmethod py-dunder [:mul Polynomial] [_ a b] (Polynomial-mul (atom a) b))

(defmethod py-dunder [:eq Polynomial] [_ a b] (Polynomial-eq (atom a) b))
