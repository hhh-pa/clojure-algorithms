(ns clojure-algorithms.maths.fibonacci
  "Calculates the Fibonacci sequence using iteration, recursion, memoization,\nand a simplified form of Binet's formula\n\nNOTE 1: the iterative, recursive, memoization functions are more accurate than\nthe Binet's formula function because the Binet formula function  uses floats\n\nNOTE 2: the Binet's formula function is much more limited in the size of inputs\nthat it can handle due to the size limitations of Python floats\nNOTE 3: the matrix function is the fastest and most memory efficient for large n\n\n\nSee benchmark numbers in __main__ for performance comparisons/\nhttps://en.wikipedia.org/wiki/Fibonacci_number for more information"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare time-func fib-iterative-yield fib-iterative fib-recursive fib-recursive-cached fib-memoization fib-binet matrix-pow-np fib-matrix-np)

(defn time-func
  "Times the execution of a function with parameters"
  [func & args]
  (let [start (System/nanoTime)
        output (apply func args)
        duration (/ (double (- (System/nanoTime) start)) 1e9)
        [time-taken unit] (cond (>= duration 1) [duration "s"]
                                (>= duration 0.001) [(* duration 1000) "ms"]
                                :else [(* duration 1000000) "µs"])]
    (println (format "%s runtime: %.4f %s"
                     (.. func getClass getSimpleName) time-taken unit))
    output))

(defn fib-iterative-yield "Calculates the first n (1-indexed) Fibonacci numbers using iteration with yield\n>>> list(fib_iterative_yield(0))\n[0]\n>>> tuple(fib_iterative_yield(1))\n(0, 1)\n>>> tuple(fib_iterative_yield(5))\n(0, 1, 1, 2, 3, 5)\n>>> tuple(fib_iterative_yield(10))\n(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)\n>>> tuple(fib_iterative_yield(-1))\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (let [__ys (atom []) --v (atom nil) a (atom nil) b (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (let [__d1 (vec [0 1])] (reset! a (nth __d1 0)) (reset! b (nth __d1 1))) (do (swap! __ys conj @a) nil) (doseq [__i2 (py-iter (range n))] (reset! --v __i2) (do (swap! __ys conj @b) nil) (let [__d3 (vec [@b (py-add @a @b)])] (reset! a (nth __d3 0)) (reset! b (nth __d3 1)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn fib-iterative "Calculates the first n (0-indexed) Fibonacci numbers using iteration\n>>> fib_iterative(0)\n[0]\n>>> fib_iterative(1)\n[0, 1]\n>>> fib_iterative(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_iterative(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_iterative(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (let [--v (atom nil) fib (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-eq n 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [0]}))) (reset! fib [0 1]) (doseq [__i1 (py-iter (range (py-sub n 1)))] (reset! --v __i1) (swap! fib conj (py-add (py-get @fib (py-neg 1)) (py-get @fib (py-neg 2))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @fib})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn fib-recursive "Calculates the first n (0-indexed) Fibonacci numbers using recursion\n>>> fib_recursive(0)\n[0]\n>>> fib_recursive(1)\n[0, 1]\n>>> fib_recursive(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_recursive(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_recursive(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (try (letfn [(fib-recursive-term [i] (try (when (py-truthy (py-lt i 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-lt i 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value i}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (fib-recursive-term (py-sub i 1)) (fib-recursive-term (py-sub i 2)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))] (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (fib-recursive-term i)))}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn fib-recursive-cached "Calculates the first n (0-indexed) Fibonacci numbers using recursion\n>>> fib_recursive_cached(0)\n[0]\n>>> fib_recursive_cached(1)\n[0, 1]\n>>> fib_recursive_cached(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_recursive_cached(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_recursive_cached(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n]
  (when (neg? n)
    (throw (ex-info "n is negative" {:type :ValueError})))
  (let [cache (atom {0 0 1 1})
        term (fn term [i]
               (if-let [v (get @cache i)]
                 v
                 (let [v (+' (term (dec i)) (term (- i 2)))]
                   (swap! cache assoc i v)
                   v)))]
    (mapv term (range (inc n)))))

(defn fib-memoization "Calculates the first n (0-indexed) Fibonacci numbers using memoization\n>>> fib_memoization(0)\n[0]\n>>> fib_memoization(1)\n[0, 1]\n>>> fib_memoization(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_memoization(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_memoization(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (let [cache (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (reset! cache {0 0 1 1 2 1}) (letfn [(rec-fn-memoized [num] (let [value (atom nil)] (try (when (py-truthy (py-in num @cache)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @cache num)}))) (reset! value (py-add (rec-fn-memoized (py-sub num 1)) (rec-fn-memoized (py-sub num 2)))) (swap! cache (fn [__c] (py-assoc-in __c [num] @value))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))] (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (rec-fn-memoized i)))}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn fib-binet "Calculates the first n (0-indexed) Fibonacci numbers using a simplified form\nof Binet's formula:\nhttps://en.m.wikipedia.org/wiki/Fibonacci_number#Computation_by_rounding\n\nNOTE 1: this function diverges from fib_iterative at around n = 71, likely\ndue to compounding floating-point arithmetic errors\n\nNOTE 2: this function doesn't accept n >= 1475 because it overflows\nthereafter due to the size limitations of Python floats\n>>> fib_binet(0)\n[0]\n>>> fib_binet(1)\n[0, 1]\n>>> fib_binet(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_binet(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_binet(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative\n>>> fib_binet(1475)\nTraceback (most recent call last):\n    ...\nValueError: n is too large"
  [n] (let [phi (atom nil) sqrt-5 (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-ge n 1475)) (throw (ex-info (py-str "n is too large") {:type :ValueError}))) (reset! sqrt-5 (Math/sqrt 5)) (reset! phi (py-div (py-add 1 @sqrt-5) 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (py-round (py-div (py-pow @phi i) @sqrt-5))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn matrix-pow-np "Raises a matrix to the power of 'power' using binary exponentiation.\n\nArgs:\n    m: Matrix as nested vectors (pure-Clojure port of the numpy version).\n    power: The power to which the matrix is to be raised.\n\nReturns:\n    The matrix raised to the power.\n\nRaises:\n    ValueError: If power is negative.\n\n>>> m = np.array([[1, 1], [1, 0]], dtype=int)\n>>> matrix_pow_np(m, 0)  # Identity matrix when raised to the power of 0\narray([[1, 0],\n       [0, 1]])\n\n>>> matrix_pow_np(m, 1)  # Same matrix when raised to the power of 1\narray([[1, 1],\n       [1, 0]])\n\n>>> matrix_pow_np(m, 5)\narray([[8, 5],\n       [5, 3]])\n\n>>> matrix_pow_np(m, -1)\nTraceback (most recent call last):\n    ...\nValueError: power is negative"
  [m power]
  (when (neg? power)
    (throw (ex-info "power is negative" {:type :ValueError})))
  (let [n (count m)
        ident (mapv (fn [i] (mapv (fn [j] (if (= i j) 1 0)) (range n)))
                    (range n))
        mmult (fn [a b]
                (let [cols (apply mapv vector b)]
                  (mapv (fn [row]
                          (mapv (fn [col] (reduce +' (map *' row col))) cols))
                        a)))]
    (loop [result ident base (mapv vec m) p power]
      (if (zero? p)
        result
        (recur (if (odd? p) (mmult result base) result)
               (mmult base base)
               (quot p 2))))))

(defn fib-matrix-np "Calculates the n-th Fibonacci number using matrix exponentiation.\nhttps://www.nayuki.io/page/fast-fibonacci-algorithms#:~:text=\nSummary:%20The%20two%20fast%20Fibonacci%20algorithms%20are%20matrix\n\nArgs:\n    n: Fibonacci sequence index\n\nReturns:\n    The n-th Fibonacci number.\n\nRaises:\n    ValueError: If n is negative.\n\n>>> fib_matrix_np(0)\n0\n>>> fib_matrix_np(1)\n1\n>>> fib_matrix_np(5)\n5\n>>> fib_matrix_np(10)\n55\n>>> fib_matrix_np(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n]
  (when (neg? n)
    (throw (ex-info "n is negative" {:type :ValueError})))
  (if (zero? n)
    0
    (get-in (matrix-pow-np [[1 1] [1 0]] (dec n)) [0 0])))
