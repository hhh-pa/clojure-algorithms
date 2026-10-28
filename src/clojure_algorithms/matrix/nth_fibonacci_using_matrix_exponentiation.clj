(ns clojure-algorithms.matrix.nth-fibonacci-using-matrix-exponentiation
  "Implementation of finding nth fibonacci number using matrix exponentiation.\nTime Complexity is about O(log(n)*8), where 8 is the complexity of matrix\nmultiplication of size 2 by 2.\nAnd on the other hand complexity of bruteforce solution is O(n).\nAs we know\n    f[n] = f[n-1] + f[n-1]\nConverting to matrix,\n    [f(n),f(n-1)] = [[1,1],[1,0]] * [f(n-1),f(n-2)]\n->  [f(n),f(n-1)] = [[1,1],[1,0]]^2 * [f(n-2),f(n-3)]\n    ...\n    ...\n->  [f(n),f(n-1)] = [[1,1],[1,0]]^(n-1) * [f(1),f(0)]\nSo we just need the n times multiplication of the matrix [1,1],[1,0]].\nWe can decrease the n times multiplication by following the divide and conquer approach."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare multiply identity nth-fibonacci-matrix nth-fibonacci-bruteforce main)

(defn multiply
  [matrix-a matrix-b] (let [i (atom nil) j (atom nil) k (atom nil) list-1 (atom nil) matrix-c (atom nil) n (atom nil) val (atom nil)] (try (reset! matrix-c []) (reset! n (count matrix-a)) (doseq [__i1 (py-iter (range @n))] (reset! i __i1) (reset! list-1 []) (doseq [__i2 (py-iter (range @n))] (reset! j __i2) (reset! val 0) (doseq [__i3 (py-iter (range @n))] (reset! k __i3) (reset! val (py-add @val (py-mul (py-get (py-get matrix-a @i) @k) (py-get (py-get matrix-b @k) @j))))) (swap! list-1 conj @val)) (swap! matrix-c conj @list-1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix-c})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn identity
  [n] (vec (for [row (py-iter (range n))] (vec (for [column (py-iter (range n))] (py-int (py-eq row column) nil))))))

(defn nth-fibonacci-matrix ">>> nth_fibonacci_matrix(100)\n354224848179261915075\n>>> nth_fibonacci_matrix(-100)\n-100"
  [n] (let [n (atom n)] (let [fibonacci-matrix (atom nil) res-matrix (atom nil)] (try (when (py-truthy (py-le @n 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @n}))) (reset! res-matrix (identity 2)) (reset! fibonacci-matrix [[1 1] [1 0]]) (reset! n (py-sub @n 1)) (while (py-truthy (py-gt @n 0)) (when (py-truthy (py-eq (py-mod @n 2) 1)) (reset! res-matrix (multiply @res-matrix @fibonacci-matrix))) (reset! fibonacci-matrix (multiply @fibonacci-matrix @fibonacci-matrix)) (reset! n (py-int (py-div @n 2) nil))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get @res-matrix 0) 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn nth-fibonacci-bruteforce ">>> nth_fibonacci_bruteforce(100)\n354224848179261915075\n>>> nth_fibonacci_bruteforce(-100)\n-100"
  [n] (let [--v (atom nil) fib0 (atom nil) fib1 (atom nil)] (try (when (py-truthy (py-le n 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value n}))) (reset! fib0 0) (reset! fib1 1) (doseq [__i1 (py-iter (range 2 (py-add n 1)))] (reset! --v __i1) (let [__d2 (vec [@fib1 (py-add @fib0 @fib1)])] (reset! fib0 (nth __d2 0)) (reset! fib1 (nth __d2 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @fib1})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [] (let [n (atom nil) ordinal (atom nil)] (doseq [__i1 (py-iter (py-split "0th 1st 2nd 3rd 10th 100th 1000th"))] (reset! ordinal __i1) (reset! n (py-int (clojure.string/join "" (map py-str (doall (for [c (py-iter @ordinal) :when (py-truthy (py-in c "0123456789"))] c)))) nil)) (println (clojure.string/join " " [(py-str (str (py-str @ordinal) " fibonacci number using matrix exponentiation is " (py-str (nth-fibonacci-matrix @n)) " and using bruteforce is " (py-str (nth-fibonacci-bruteforce @n)) "\n"))]))) nil))
