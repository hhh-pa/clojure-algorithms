(ns clojure-algorithms.machine-learning.automatic-differentiation
  "Demonstration of the Automatic Differentiation (Reverse mode).\n\nReference: https://en.wikipedia.org/wiki/Automatic_differentiation\n\nAuthor: Poojan Smart\nEmail: smrtpoojan@gmail.com"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare OpType OpType-ADD OpType-SUB OpType-MUL OpType-DIV OpType-MATMUL OpType-POWER OpType-NOOP make-OpType make-Variable Variable-repr Variable-to-ndarray Variable-add Variable-sub Variable-mul Variable-truediv Variable-matmul Variable-pow Variable-add-param-to Variable-add-result-of make-Operation Operation-add-params Operation-add-output Operation-eq make-GradientTracker GradientTracker-enter GradientTracker-exit GradientTracker-append GradientTracker-gradient GradientTracker-derivative GradientTracker-instance)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-div py-pow py-matmul py-float-str py-quote py-repr py-str)

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

(defn- py-matmul [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :matmul a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for @" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :matmul-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for @" {:type :TypeError})) r))
        (and (sequential? a) (sequential? b))
        (let [row? (fn [x] (and (sequential? x) (every? number? x)))]
          (cond (and (row? a) (row? b)) (reduce +' 0 (map *' a b))
                (and (sequential? (first a)) (row? b))
                (mapv (fn [row] (reduce +' 0 (map *' row b))) a)
                (and (row? a) (sequential? (first b)))
                (mapv (fn [j] (reduce +' 0 (map *' a (map #(nth % j) b)))) (range (count (first b))))
                :else (let [bt (apply mapv vector b)]
                        (mapv (fn [row] (mapv (fn [col] (reduce +' 0 (map *' row col))) bt)) a))))
        :else (throw (ex-info "unsupported operand for @" {:type :TypeError}))))

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

(defrecord Variable [value param-to result-of])

(defrecord Operation [op-type other-params params output])

(defrecord GradientTracker [enabled instance])

(defn make-OpType [v] (get (into {} (map (fn [[k w]] [w k])) OpType) v v))

(defn make-Variable "Constructor for Variable. Class represents n-dimensional object which is used to wrap numpy array on which\noperations will be performed and the gradient will be calculated.\n\nExamples:\n>>> Variable(5.0)\nVariable(5.0)\n>>> Variable([5.0, 2.9])\nVariable([5.  2.9])\n>>> Variable([5.0, 2.9]) + Variable([1.0, 5.5])\nVariable([6.  8.4])\n>>> Variable([[8.0, 10.0]])\nVariable([[ 8. 10.]])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn make-Operation "Constructor for Operation. Class represents operation between single or two Variable objects.\nOperation objects contains type of operation, pointers to input Variable\nobjects and pointer to resulting Variable from the operation."
  ([op-type] (make-Operation op-type nil))
  ([op-type other-params] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:op-type] op-type))) (swap! self (fn [__c] (py-assoc-in __c [:other-params] (if (py-truthy (nil? other-params)) {} other-params)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Operation @self))))

(defn make-GradientTracker "Constructor for GradientTracker. Class contains methods to compute partial derivatives of Variable\nbased on the computation graph.\n\nExamples:\n\n>>> with GradientTracker() as tracker:\n...     a = Variable([2.0, 5.0])\n...     b = Variable([1.0, 2.0])\n...     m = Variable([1.0, 2.0])\n...     c = a + b\n...     d = a * b\n...     e = c / d\n>>> tracker.gradient(e, a)\narray([-0.25, -0.04])\n>>> tracker.gradient(e, b)\narray([-1.  , -0.25])\n>>> tracker.gradient(e, m) is None\nTrue\n\n>>> with GradientTracker() as tracker:\n...     a = Variable([[2.0, 5.0]])\n...     b = Variable([[1.0], [2.0]])\n...     c = a @ b\n>>> tracker.gradient(c, a)\narray([[1., 2.]])\n>>> tracker.gradient(c, b)\narray([[2.],\n       [5.]])\n\n>>> with GradientTracker() as tracker:\n...     a = Variable([[2.0, 5.0]])\n...     b = a ** 3\n>>> tracker.gradient(b, a)\narray([[12., 75.]])"
  [] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:enabled] false))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->GradientTracker @self)))

(defn Variable-repr
  [self] (str "Variable(" (py-str (:value @self)) ")"))

(defn Variable-to-ndarray
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Variable-add
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-add (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-ADD)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-sub
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-sub (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-SUB)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-mul
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-mul (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-MUL)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-truediv
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-div (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-DIV)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-matmul
  [self other] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-matmul (:value @self) (:value other)))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-MATMUL)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-pow
  [self power] (let [result (atom nil) tracker (atom nil)] (try (reset! result (make-Variable (py-pow (:value @self) power))) (let [__w1 (atom (make-GradientTracker))] (reset! tracker (GradientTracker-enter __w1)) (when (py-truthy (:enabled @tracker)) (swap! tracker conj OpType-POWER)) (GradientTracker-exit __w1 nil nil nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Variable-add-param-to
  [self param-to] (conj (vec (:param-to @self)) param-to) nil)

(defn Variable-add-result-of
  [self result-of] (swap! self (fn [__c] (py-assoc-in __c [:result-of] result-of))) nil)

(defn Operation-add-params
  [self params] (swap! self (fn [__c] (py-assoc-in __c [:params] params))) nil)

(defn Operation-add-output
  [self output] (swap! self (fn [__c] (py-assoc-in __c [:output] output))) nil)

(defn Operation-eq
  [self value] (if (py-truthy (contains? (set (vals OpType)) value)) (py-eq (:op-type @self) value) false))

(defn GradientTracker-enter
  [self] (try (swap! self (fn [__c] (py-assoc-in __c [:enabled] true))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @self})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn GradientTracker-exit
  [self exc-type exc traceback] (swap! self (fn [__c] (py-assoc-in __c [:enabled] false))) nil)

(defn GradientTracker-append "Adds Operation object to the related Variable objects for\ncreating computational graph for calculating gradients.\n\nArgs:\n    op_type: Operation type\n    params: Input parameters to the operation\n    output: Output variable of the operation"
  ([self op-type params output] (GradientTracker-append self op-type params output nil))
  ([self op-type params output other-params] (let [operation (atom nil) param (atom nil) param-nodes (atom nil)] (reset! operation (make-Operation op-type nil)) (reset! param-nodes []) (doseq [__i1 (py-iter params)] (reset! param __i1) (Variable-add-param-to param @operation) (swap! param-nodes conj @param)) (Variable-add-result-of (atom output) @operation) (Operation-add-params operation @param-nodes) (Operation-add-output operation output) nil)))

(defn GradientTracker-gradient "Reverse accumulation of partial derivatives to calculate gradients\nof target variable with respect to source variable.\n\nArgs:\n    target: target variable for which gradients are calculated.\n    source: source variable with respect to which the gradients are\n    calculated.\n\nReturns:\n    Gradient of the source variable with respect to the target variable"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn GradientTracker-derivative "Compute the derivative of given operation/function\n\nArgs:\n    param: variable to be differentiated\n    operation: function performed on the input variable\n\nReturns:\n    Derivative of input variable with respect to the output of\n    the operation"
  [& _args]
  ;; stub: original requires np,numpy-attr:T; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T" {:type :UnsupportedOperation})))

(def OpType
  "Class represents list of supported operations on Variable for gradient calculation."
  {:ADD 0 :SUB 1 :MUL 2 :DIV 3 :MATMUL 4 :POWER 5 :NOOP 6})

(def OpType-ADD 0)

(def OpType-SUB 1)

(def OpType-MUL 2)

(def OpType-DIV 3)

(def OpType-MATMUL 4)

(def OpType-POWER 5)

(def OpType-NOOP 6)

(def GradientTracker-instance nil)

(defmethod print-method Variable [o w] (.write w (Variable-repr (atom o))))

(defmethod py-dunder [:add Variable] [_ a b] (Variable-add (atom a) b))

(defmethod py-dunder [:sub Variable] [_ a b] (Variable-sub (atom a) b))

(defmethod py-dunder [:mul Variable] [_ a b] (Variable-mul (atom a) b))

(defmethod py-dunder [:div Variable] [_ a b] (Variable-truediv (atom a) b))

(defmethod py-dunder [:matmul Variable] [_ a b] (Variable-matmul (atom a) b))

(defmethod py-dunder [:pow Variable] [_ a b] (Variable-pow (atom a) b))

(defmethod py-dunder [:eq Operation] [_ a b] (Operation-eq (atom a) b))

;; NOTE: Variable __init__ stubbed (np)
;; NOTE: Variable.to_ndarray stubbed (np)
;; NOTE: GradientTracker.gradient stubbed (np)
;; NOTE: GradientTracker.derivative stubbed (np,numpy-attr:T)
