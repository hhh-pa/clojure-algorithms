(ns clojure-algorithms.maths.fibonacci
  "Calculates the Fibonacci sequence using iteration, recursion, memoization,\nand a simplified form of Binet's formula\n\nNOTE 1: the iterative, recursive, memoization functions are more accurate than\nthe Binet's formula function because the Binet formula function  uses floats\n\nNOTE 2: the Binet's formula function is much more limited in the size of inputs\nthat it can handle due to the size limitations of Python floats\nNOTE 3: the matrix function is the fastest and most memory efficient for large n\n\n\nSee benchmark numbers in __main__ for performance comparisons/\nhttps://en.wikipedia.org/wiki/Fibonacci_number for more information"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare time-func fib-iterative-yield fib-iterative fib-recursive fib-recursive-cached fib-memoization fib-binet matrix-pow-np fib-matrix-np)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-div py-pow py-neg py-float-str py-quote py-repr py-str py-round)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

(defn- py-round
  ([x] (long (Math/rint (double x))))
  ([x n]
   (if (nil? n) (py-round x)
       (let [nn (long n)]
         (if (>= nn 0)
           (let [f (Math/pow 10.0 (double nn))]
             (/ (Math/rint (* (double x) f)) f))
           (let [f (Math/pow 10.0 (double (- nn)))]
             (* (Math/rint (/ (double x) f)) f)))))))

(defn time-func "Times the execution of a function with parameters"
  [& _args]
  ;; stub: original requires kwargs for func; pure-Clojure port not applicable
  (throw (ex-info "requires kwargs for func" {:type :UnsupportedOperation})))

(defn fib-iterative-yield "Calculates the first n (1-indexed) Fibonacci numbers using iteration with yield\n>>> list(fib_iterative_yield(0))\n[0]\n>>> tuple(fib_iterative_yield(1))\n(0, 1)\n>>> tuple(fib_iterative_yield(5))\n(0, 1, 1, 2, 3, 5)\n>>> tuple(fib_iterative_yield(10))\n(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)\n>>> tuple(fib_iterative_yield(-1))\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (let [__ys (atom []) --v (atom nil) a (atom nil) b (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (let [__d1 (vec [0 1])] (reset! a (nth __d1 0)) (reset! b (nth __d1 1))) (do (swap! __ys conj @a) nil) (doseq [__i2 (py-iter (range n))] (reset! --v __i2) (do (swap! __ys conj @b) nil) (let [__d3 (vec [@b (py-add @a @b)])] (reset! a (nth __d3 0)) (reset! b (nth __d3 1)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn fib-iterative "Calculates the first n (0-indexed) Fibonacci numbers using iteration\n>>> fib_iterative(0)\n[0]\n>>> fib_iterative(1)\n[0, 1]\n>>> fib_iterative(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_iterative(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_iterative(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (let [--v (atom nil) fib (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-eq n 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [0]}))) (reset! fib [0 1]) (doseq [__i1 (py-iter (range (py-sub n 1)))] (reset! --v __i1) (swap! fib conj (py-add (py-get @fib (py-neg 1)) (py-get @fib (py-neg 2))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @fib})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn fib-recursive "Calculates the first n (0-indexed) Fibonacci numbers using recursion\n>>> fib_recursive(0)\n[0]\n>>> fib_recursive(1)\n[0, 1]\n>>> fib_recursive(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_recursive(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_recursive(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (try (letfn [(fib-recursive-term [i] (try (when (py-truthy (py-lt i 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-lt i 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value i}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (fib-recursive-term (py-sub i 1)) (fib-recursive-term (py-sub i 2)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))] (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (fib-recursive-term i)))}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn fib-recursive-cached "Calculates the first n (0-indexed) Fibonacci numbers using recursion\n>>> fib_recursive_cached(0)\n[0]\n>>> fib_recursive_cached(1)\n[0, 1]\n>>> fib_recursive_cached(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_recursive_cached(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_recursive_cached(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (try (letfn [(fib-recursive-term [i] (try (when (py-truthy (py-lt i 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-lt i 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value i}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (fib-recursive-term (py-sub i 1)) (fib-recursive-term (py-sub i 2)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))] (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (fib-recursive-term i)))}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn fib-memoization "Calculates the first n (0-indexed) Fibonacci numbers using memoization\n>>> fib_memoization(0)\n[0]\n>>> fib_memoization(1)\n[0, 1]\n>>> fib_memoization(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_memoization(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_memoization(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [n] (let [cache (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (reset! cache {0 0 1 1 2 1}) (letfn [(rec-fn-memoized [num] (let [cache (atom nil) value (atom nil)] (try (when (py-truthy (py-in num @cache)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @cache num)}))) (reset! value (py-add (rec-fn-memoized (py-sub num 1)) (rec-fn-memoized (py-sub num 2)))) (swap! cache (fn [__c] (py-assoc-in __c [num] @value))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))] (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (rec-fn-memoized i)))}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn fib-binet "Calculates the first n (0-indexed) Fibonacci numbers using a simplified form\nof Binet's formula:\nhttps://en.m.wikipedia.org/wiki/Fibonacci_number#Computation_by_rounding\n\nNOTE 1: this function diverges from fib_iterative at around n = 71, likely\ndue to compounding floating-point arithmetic errors\n\nNOTE 2: this function doesn't accept n >= 1475 because it overflows\nthereafter due to the size limitations of Python floats\n>>> fib_binet(0)\n[0]\n>>> fib_binet(1)\n[0, 1]\n>>> fib_binet(5)\n[0, 1, 1, 2, 3, 5]\n>>> fib_binet(10)\n[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]\n>>> fib_binet(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative\n>>> fib_binet(1475)\nTraceback (most recent call last):\n    ...\nValueError: n is too large"
  [n] (let [phi (atom nil) sqrt-5 (atom nil)] (try (when (py-truthy (py-lt n 0)) (throw (ex-info (py-str "n is negative") {:type :ValueError}))) (when (py-truthy (py-ge n 1475)) (throw (ex-info (py-str "n is too large") {:type :ValueError}))) (reset! sqrt-5 (Math/sqrt 5)) (reset! phi (py-div (py-add 1 @sqrt-5) 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range (py-add n 1)))] (py-round (py-div (py-pow @phi i) @sqrt-5))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn matrix-pow-np "Raises a matrix to the power of 'power' using binary exponentiation.\n\nArgs:\n    m: Matrix as a numpy array.\n    power: The power to which the matrix is to be raised.\n\nReturns:\n    The matrix raised to the power.\n\nRaises:\n    ValueError: If power is negative.\n\n>>> m = np.array([[1, 1], [1, 0]], dtype=int)\n>>> matrix_pow_np(m, 0)  # Identity matrix when raised to the power of 0\narray([[1, 0],\n       [0, 1]])\n\n>>> matrix_pow_np(m, 1)  # Same matrix when raised to the power of 1\narray([[1, 1],\n       [1, 0]])\n\n>>> matrix_pow_np(m, 5)\narray([[8, 5],\n       [5, 3]])\n\n>>> matrix_pow_np(m, -1)\nTraceback (most recent call last):\n    ...\nValueError: power is negative"
  [& _args]
  ;; stub: original requires ndarray,np,numpy-attr:dot; pure-Clojure port not applicable
  (throw (ex-info "requires ndarray,np,numpy-attr:dot" {:type :UnsupportedOperation})))

(defn fib-matrix-np "Calculates the n-th Fibonacci number using matrix exponentiation.\nhttps://www.nayuki.io/page/fast-fibonacci-algorithms#:~:text=\nSummary:%20The%20two%20fast%20Fibonacci%20algorithms%20are%20matrix\n\nArgs:\n    n: Fibonacci sequence index\n\nReturns:\n    The n-th Fibonacci number.\n\nRaises:\n    ValueError: If n is negative.\n\n>>> fib_matrix_np(0)\n0\n>>> fib_matrix_np(1)\n1\n>>> fib_matrix_np(5)\n5\n>>> fib_matrix_np(10)\n55\n>>> fib_matrix_np(-1)\nTraceback (most recent call last):\n    ...\nValueError: n is negative"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: time_func stubbed (kwargs for func)
;; NOTE: matrix_pow_np stubbed (ndarray,np,numpy-attr:dot)
;; NOTE: fib_matrix_np stubbed (np)
