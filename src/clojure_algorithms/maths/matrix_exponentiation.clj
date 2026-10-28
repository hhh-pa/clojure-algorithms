(ns clojure-algorithms.maths.matrix-exponentiation
  "Matrix Exponentiation is a technique to solve linear recurrences in logarithmic time.\nYou read more about it here:\nhttps://zobayer.blogspot.com/2010/11/matrix-exponentiation.html\nhttps://www.hackerearth.com/practice/notes/matrix-exponentiation-1/"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare modular-exponentiation fibonacci-with-matrix-exponentiation simple-fibonacci matrix-exponentiation-time simple-fibonacci-time main make-Matrix Matrix-mul)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-band py-float-str py-quote py-repr)

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

(defn- py-get-in [c ks]
  (if (and (instance? clojure.lang.IRecord c) (> (count ks) 1))
    (let [r (py-dunder :getitem c (vec ks))]
      (if (identical? r py-no-dunder)
        (reduce py-get c ks)
        r))
    (reduce py-get c ks)))

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

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-band [a b]
  (cond (and (boolean? a) (boolean? b)) (and a b)
        (and (set? a) (set? b)) (clojure.set/intersection a b)
        (and (integer? (py-num-bool a)) (integer? (py-num-bool b)))
        (.and (.toBigInteger (bigint (py-num-bool a)))
              (.toBigInteger (bigint (py-num-bool b))))
        (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :band a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for &" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :band-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for &" {:type :TypeError})) r))
        :else (throw (ex-info "unsupported operand for &" {:type :TypeError}))))

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
