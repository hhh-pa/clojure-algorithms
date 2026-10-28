(ns clojure-algorithms.maths.matrix-exponentiation
  "Matrix Exponentiation is a technique to solve linear recurrences in logarithmic time.\nYou read more about it here:\nhttps://zobayer.blogspot.com/2010/11/matrix-exponentiation.html\nhttps://www.hackerearth.com/practice/notes/matrix-exponentiation-1/"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare modular-exponentiation fibonacci-with-matrix-exponentiation simple-fibonacci matrix-exponentiation-time simple-fibonacci-time main make-Matrix Matrix-mul)

(defrecord Matrix [t n])

(defn make-Matrix "Constructor for Matrix."
  [arg] (let [self (atom {})] (try (if (py-truthy (sequential? arg)) (do (swap! self (fn [__c] (py-assoc-in __c [:t] arg))) (swap! self (fn [__c] (py-assoc-in __c [:n] (count arg))))) (do (swap! self (fn [__c] (py-assoc-in __c [:n] arg))) (swap! self (fn [__c] (py-assoc-in __c [:t] (vec (for [--v (py-iter (range (:n @self)))] (vec (for [--v (py-iter (range (:n @self)))] 0))))))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Matrix @self)))

(defn Matrix-mul
  [self b] (let [i (atom nil) j (atom nil) k (atom nil) matrix (atom nil)] (try (reset! matrix (make-Matrix (:n @self))) (doseq [__i1 (py-iter (range (:n @self)))] (reset! i __i1) (doseq [__i2 (py-iter (range (:n @self)))] (reset! j __i2) (doseq [__i3 (py-iter (range (:n @self)))] (reset! k __i3) (swap! matrix (fn [__c] (py-assoc-in __c [:t @i @j] (py-add (py-get-in __c [:t @i @j]) (py-mul (py-get (py-get (:t @self) @i) @k) (py-get (py-get (:t b) @k) @j))))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn modular-exponentiation
  [a b] (let [a (atom a) b (atom b)] (let [matrix (atom nil)] (try (reset! matrix (make-Matrix [[1 0] [0 1]])) (while (py-truthy (py-gt @b 0)) (when (py-truthy (py-band @b 1)) (swap! matrix py-mul @a)) (swap! a py-mul @a) (swap! b (fn [__o] (.shiftRight (.toBigInteger (bigint __o)) (int 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn fibonacci-with-matrix-exponentiation "Returns the nth number of the Fibonacci sequence that\nstarts with f1 and f2\nUses the matrix exponentiation\n>>> fibonacci_with_matrix_exponentiation(1, 5, 6)\n5\n>>> fibonacci_with_matrix_exponentiation(2, 10, 11)\n11\n>>> fibonacci_with_matrix_exponentiation(13, 0, 1)\n144\n>>> fibonacci_with_matrix_exponentiation(10, 5, 9)\n411\n>>> fibonacci_with_matrix_exponentiation(9, 2, 3)\n89"
  [n f1 f2] (let [matrix (atom nil)] (try (if (py-truthy (py-eq n 1)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value f1}))) (do (when (py-truthy (py-eq n 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value f2}))))) (reset! matrix (make-Matrix [[1 1] [1 0]])) (reset! matrix (modular-exponentiation @matrix (py-sub n 2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-mul f2 (py-get (py-get (:t @matrix) 0) 0)) (py-mul f1 (py-get (py-get (:t @matrix) 0) 1)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn simple-fibonacci "Returns the nth number of the Fibonacci sequence that\nstarts with f1 and f2\nUses the definition\n>>> simple_fibonacci(1, 5, 6)\n5\n>>> simple_fibonacci(2, 10, 11)\n11\n>>> simple_fibonacci(13, 0, 1)\n144\n>>> simple_fibonacci(10, 5, 9)\n411\n>>> simple_fibonacci(9, 2, 3)\n89"
  [n f1 f2] (let [n (atom n) f1 (atom f1) f2 (atom f2)] (try (if (py-truthy (py-eq @n 1)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value @f1}))) (do (when (py-truthy (py-eq @n 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @f2}))))) (swap! n py-sub 2) (while (py-truthy (py-gt @n 0)) (let [__d1 (vec [(py-add @f1 @f2) @f2])] (reset! f2 (nth __d1 0)) (reset! f1 (nth __d1 1))) (swap! n py-sub 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @f2})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn matrix-exponentiation-time
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

(defn simple-fibonacci-time
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

(defn main
  [] (matrix-exponentiation-time) (simple-fibonacci-time) nil)

(defmethod py-dunder [:mul Matrix] [_ a b] (Matrix-mul (atom a) b))

;; NOTE: matrix_exponentiation_time stubbed (timeit)
;; NOTE: simple_fibonacci_time stubbed (timeit)
