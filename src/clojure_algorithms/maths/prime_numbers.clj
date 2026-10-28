(ns clojure-algorithms.maths.prime-numbers
  "Port of maths/prime_numbers.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare slow-primes primes fast-primes benchmark)

(defn slow-primes "Return a list of all primes numbers up to max.\n>>> list(slow_primes(0))\n[]\n>>> list(slow_primes(-1))\n[]\n>>> list(slow_primes(-10))\n[]\n>>> list(slow_primes(25))\n[2, 3, 5, 7, 11, 13, 17, 19, 23]\n>>> list(slow_primes(11))\n[2, 3, 5, 7, 11]\n>>> list(slow_primes(33))\n[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]\n>>> list(slow_primes(1000))[-1]\n997"
  [max-n] (let [__ys (atom []) i (atom nil) j (atom nil) numbers (atom nil)] (try (reset! numbers (doall (for [i (py-iter (range 1 (py-add max-n 1)))] i))) (doseq [__i1 (py-iter (doall (for [n (py-iter @numbers) :when (py-truthy (py-gt n 1))] n)))] (reset! i __i1) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 2 @i))] (reset! j __i2) (when (py-truthy (py-eq (py-mod @i @j) 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (do (swap! __ys conj @i) nil)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn primes "Return a list of all primes numbers up to max.\n>>> list(primes(0))\n[]\n>>> list(primes(-1))\n[]\n>>> list(primes(-10))\n[]\n>>> list(primes(25))\n[2, 3, 5, 7, 11, 13, 17, 19, 23]\n>>> list(primes(11))\n[2, 3, 5, 7, 11]\n>>> list(primes(33))\n[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]\n>>> list(primes(1000))[-1]\n997"
  [max-n] (let [__ys (atom []) bound (atom nil) i (atom nil) j (atom nil) numbers (atom nil)] (try (reset! numbers (doall (for [i (py-iter (range 1 (py-add max-n 1)))] i))) (doseq [__i1 (py-iter (doall (for [n (py-iter @numbers) :when (py-truthy (py-gt n 1))] n)))] (reset! i __i1) (reset! bound (py-add (py-int (Math/sqrt @i) nil) 1)) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 2 @bound))] (reset! j __i2) (when (py-truthy (py-eq (py-mod @i @j) 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (do (swap! __ys conj @i) nil)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn fast-primes "Return a list of all primes numbers up to max.\n>>> list(fast_primes(0))\n[]\n>>> list(fast_primes(-1))\n[]\n>>> list(fast_primes(-10))\n[]\n>>> list(fast_primes(25))\n[2, 3, 5, 7, 11, 13, 17, 19, 23]\n>>> list(fast_primes(11))\n[2, 3, 5, 7, 11]\n>>> list(fast_primes(33))\n[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]\n>>> list(fast_primes(1000))[-1]\n997"
  [max-n] (let [__ys (atom []) bound (atom nil) i (atom nil) j (atom nil) numbers (atom nil)] (try (reset! numbers (doall (for [i (py-iter (range 1 (py-add max-n 1) 2))] i))) (when (py-truthy (py-gt max-n 2)) (do (swap! __ys conj 2) nil)) (doseq [__i1 (py-iter (doall (for [n (py-iter @numbers) :when (py-truthy (py-gt n 1))] n)))] (reset! i __i1) (reset! bound (py-add (py-int (Math/sqrt @i) nil) 1)) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 3 @bound 2))] (reset! j __i2) (when (py-truthy (py-eq (py-mod @i @j) 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (do (swap! __ys conj @i) nil)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn benchmark "Let's benchmark our functions side-by-side..."
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

;; NOTE: benchmark stubbed (timeit)
