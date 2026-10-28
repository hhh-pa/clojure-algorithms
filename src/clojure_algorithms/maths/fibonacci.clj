(ns clojure-algorithms.maths.fibonacci
  "Calculates the Fibonacci sequence using iteration, recursion, memoization,
  and a simplified form of Binet's formula

  NOTE 1: the iterative, recursive, memoization functions are more accurate than
  the Binet's formula function because the Binet formula function uses floats

  NOTE 2: the Binet's formula function is much more limited in the size of inputs
  that it can handle due to the size limitations of Python floats
  NOTE 3: the matrix function is the fastest and most memory efficient for large n


  See benchmark numbers in __main__ for performance comparisons/
  https://en.wikipedia.org/wiki/Fibonacci_number for more information")

(defn- check-n
  [n]
  (when (neg? n)
    (throw (ex-info "n is negative" {:type :ValueError}))))

(defn time-func
  "Times the execution of a function with parameters, prints the
  runtime, and returns the function's output."
  [func & args]
  (let [start (System/nanoTime)
        output (apply func args)
        duration (/ (double (- (System/nanoTime) start)) 1e9)
        [time-taken unit] (cond (>= duration 1) [duration "s"]
                                (>= duration 0.001) [(* duration 1000) "ms"]
                                :else [(* duration 1000000) "µs"])]
    (println (format "%s runtime: %.4f %s"
                     (.getSimpleName (class func)) time-taken unit))
    output))

(defn fib-iterative-yield
  "Returns a lazy seq of the first n (1-indexed) Fibonacci numbers
  using iteration (port of the Python generator)."
  [n]
  (check-n n)
  (map first (take (inc n) (iterate (fn [[a b]] [b (+' a b)]) [0 1]))))

(defn fib-iterative
  "Calculates the first n (0-indexed) Fibonacci numbers using iteration."
  [n]
  (check-n n)
  (if (zero? n)
    [0]
    (loop [i 1 fib [0 1]]
      (if (>= i n)
        fib
        (recur (inc i) (conj fib (+' (peek fib) (fib (- (count fib) 2)))))))))

(defn- fib-recursive-term
  [i]
  (if (< i 2)
    i
    (+' (fib-recursive-term (dec i)) (fib-recursive-term (- i 2)))))

(defn fib-recursive
  "Calculates the first n (0-indexed) Fibonacci numbers using recursion."
  [n]
  (check-n n)
  (mapv fib-recursive-term (range (inc n))))

(declare fib-term-cached)

(defn- fib-term-uncached
  [i]
  (if (< i 2)
    i
    (+' (fib-term-cached (dec i)) (fib-term-cached (- i 2)))))

(def fib-term-cached
  "Memoized i-th (0-indexed) Fibonacci number."
  (memoize fib-term-uncached))

(defn fib-recursive-cached
  "Calculates the first n (0-indexed) Fibonacci numbers using recursion
  with memoization."
  [n]
  (check-n n)
  (mapv fib-term-cached (range (inc n))))

(defn fib-memoization
  "Calculates the first n (0-indexed) Fibonacci numbers using memoization."
  [n]
  (check-n n)
  (loop [i 0 cache {0 0 1 1 2 1} out []]
    (if (> i n)
      out
      (let [v (if (contains? cache i)
                (cache i)
                (+' (cache (dec i)) (cache (- i 2))))]
        (recur (inc i) (assoc cache i v) (conj out v))))))

(defn fib-binet
  "Calculates the first n (0-indexed) Fibonacci numbers using a simplified
  form of Binet's formula:
  https://en.m.wikipedia.org/wiki/Fibonacci_number#Computation_by_rounding"
  [n]
  (check-n n)
  (when (>= n 1475)
    (throw (ex-info "n is too large" {:type :ValueError})))
  (let [sqrt-5 (Math/sqrt 5)
        phi (/ (+ 1 sqrt-5) 2)]
    (mapv (fn [i] (Math/round (/ (Math/pow phi i) sqrt-5)))
          (range (inc n)))))

(defn- matrix-mult
  "Multiplies two matrices represented as nested vectors."
  [a b]
  (let [cols (apply mapv vector b)]
    (mapv (fn [row]
            (mapv (fn [col] (reduce +' (map *' row col))) cols))
          a)))

(defn matrix-pow-np
  "Raises a matrix (nested vectors; pure-Clojure port of the numpy
  version) to the power of 'power' using binary exponentiation."
  [m power]
  (when (neg? power)
    (throw (ex-info "power is negative" {:type :ValueError})))
  (let [size (count m)
        ident (mapv (fn [i] (mapv (fn [j] (if (= i j) 1 0)) (range size)))
                    (range size))]
    (loop [result ident base (mapv vec m) p power]
      (if (zero? p)
        result
        (recur (if (odd? p) (matrix-mult result base) result)
               (matrix-mult base base)
               (quot p 2))))))

(defn fib-matrix-np
  "Calculates the n-th Fibonacci number using matrix exponentiation.
  https://www.nayuki.io/page/fast-fibonacci-algorithms"
  [n]
  (check-n n)
  (if (zero? n)
    0
    (get-in (matrix-pow-np [[1 1] [1 0]] (dec n)) [0 0])))
