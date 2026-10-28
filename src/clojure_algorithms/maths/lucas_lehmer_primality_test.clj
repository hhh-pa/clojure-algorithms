(ns clojure-algorithms.maths.lucas-lehmer-primality-test
  "In mathematics, the Lucas-Lehmer test (LLT) is a primality test for Mersenne\nnumbers.  https://en.wikipedia.org/wiki/Lucas%E2%80%93Lehmer_primality_test\n\nA Mersenne number is a number that is one less than a power of two.\nThat is M_p = 2^p - 1\nhttps://en.wikipedia.org/wiki/Mersenne_prime\n\nThe Lucas-Lehmer test is the primality test used by the\nGreat Internet Mersenne Prime Search (GIMPS) to locate large primes."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare lucas-lehmer-test)

(defn lucas-lehmer-test ">>> lucas_lehmer_test(p=7)\nTrue\n\n>>> lucas_lehmer_test(p=11)\nFalse\n\n# M_11 = 2^11 - 1 = 2047 = 23 * 89"
  [p] (let [--v (atom nil) m (atom nil) s (atom nil)] (try (when (py-truthy (py-lt p 2)) (throw (ex-info (py-str "p should not be less than 2!") {:type :ValueError}))) (when (py-truthy (py-eq p 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true}))) (reset! s 4) (reset! m (py-sub (.shiftLeft (.toBigInteger (bigint 1)) (int p)) 1)) (doseq [__i1 (py-iter (range (py-sub p 2)))] (reset! --v __i1) (reset! s (py-mod (py-sub (py-mul @s @s) 2) @m))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @s 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
