(ns clojure-algorithms.maths.polynomials.single-indeterminate-operations
  "This module implements a single indeterminate polynomials class\nwith some basic operations\n\nReference: https://en.wikipedia.org/wiki/Polynomial"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare make-Polynomial Polynomial-add Polynomial-sub Polynomial-neg Polynomial-mul Polynomial-evaluate Polynomial-str Polynomial-repr Polynomial-derivative Polynomial-integral Polynomial-eq Polynomial-ne)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-slice py-add py-sub py-repeat-vec py-mul py-div py-pow py-neg py-abs py-list py-float-str py-quote py-repr py-str)

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

(defn- py-slice [c lo hi step]
  (when (map? c) (throw (ex-info "unhashable slice" {:type :TypeError})))
  (let [s (vec (seq c)) n (count s)
        step (if (nil? step) 1 step)]
    (when (zero? step) (throw (ex-info "slice step cannot be zero" {:type :ValueError})))
    (let [idxs (if (pos? step)
                 (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                        (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                        step)
                 (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                        (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                        step))
          res (mapv s idxs)]
      (if (string? c) (apply str res) res))))

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

(defn- py-abs [x]
  (cond (boolean? x) (if x 1 0)
        (number? x) (if (neg? x) (-' x) x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :abs x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for abs()" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for abs()" {:type :TypeError}))))

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

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
