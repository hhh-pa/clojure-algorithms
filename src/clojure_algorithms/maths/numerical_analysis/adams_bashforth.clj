(ns clojure-algorithms.maths.numerical-analysis.adams-bashforth
  "Use the Adams-Bashforth methods to solve Ordinary Differential Equations.\n\nhttps://en.wikipedia.org/wiki/Linear_multistep_method\nAuthor : Ravi Kumar"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare make-AdamsBashforth AdamsBashforth-post-init AdamsBashforth-step-2 AdamsBashforth-step-3 AdamsBashforth-step-4 AdamsBashforth-step-5)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-slice py-sub py-neg py-float-str py-quote py-repr py-str py-round)

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

(defrecord AdamsBashforth [func x-initials y-initials step-size x-final])

(defn make-AdamsBashforth "Constructor for AdamsBashforth. args:\nfunc: An ordinary differential equation (ODE) as function of x and y.\nx_initials: List containing initial required values of x.\ny_initials: List containing initial required values of y.\nstep_size: The increment value of x.\nx_final: The final value of x.\n\nReturns: Solution of y at each nodal point\n\n>>> def f(x, y):\n...     return x + y\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0.2, 1], 0.2, 1)  # doctest: +ELLIPSIS\nAdamsBashforth(func=..., x_initials=[0, 0.2, 0.4], y_initials=[0, 0.2, 1], step...)\n>>> AdamsBashforth(f, [0, 0.2, 1], [0, 0, 0.04], 0.2, 1).step_2()\nTraceback (most recent call last):\n    ...\nValueError: The final value of x must be greater than the initial values of x.\n\n>>> AdamsBashforth(f, [0, 0.2, 0.3], [0, 0, 0.04], 0.2, 1).step_3()\nTraceback (most recent call last):\n    ...\nValueError: x-values must be equally spaced according to step size.\n\n>>> AdamsBashforth(f,[0,0.2,0.4,0.6,0.8],[0,0,0.04,0.128,0.307],-0.2,1).step_5()\nTraceback (most recent call last):\n    ...\nValueError: Step size must be positive."
  [func x-initials y-initials step-size x-final] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:func] func))) (swap! self (fn [__c] (py-assoc-in __c [:x-initials] x-initials))) (swap! self (fn [__c] (py-assoc-in __c [:y-initials] y-initials))) (swap! self (fn [__c] (py-assoc-in __c [:step-size] step-size))) (swap! self (fn [__c] (py-assoc-in __c [:x-final] x-final))) (AdamsBashforth-post-init self) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->AdamsBashforth @self)))

(defn AdamsBashforth-post-init
  [self] (when (py-truthy (py-ge (py-get (:x-initials @self) (py-neg 1)) (:x-final @self))) (throw (ex-info (py-str "The final value of x must be greater than the initial values of x.") {:type :ValueError}))) (when (py-truthy (py-le (:step-size @self) 0)) (throw (ex-info (py-str "Step size must be positive.") {:type :ValueError}))) (when (py-truthy (not (py-truthy (every? py-truthy (doall (for [[x0 x1] (py-iter (map vector (:x-initials @self) (py-slice (:x-initials @self) 1 nil nil)))] (py-eq (py-round (py-sub x1 x0) 10) (:step-size @self)))))))) (throw (ex-info (py-str "x-values must be equally spaced according to step size.") {:type :ValueError}))) nil)

(defn AdamsBashforth-step-2 ">>> def f(x, y):\n...     return x\n>>> AdamsBashforth(f, [0, 0.2], [0, 0], 0.2, 1).step_2()\narray([0.  , 0.  , 0.06, 0.16, 0.3 , 0.48])\n\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_2()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn AdamsBashforth-step-3 ">>> def f(x, y):\n...     return x + y\n>>> y = AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_3()\n>>> float(y[3])\n0.15533333333333332\n\n>>> AdamsBashforth(f, [0, 0.2], [0, 0], 0.2, 1).step_3()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn AdamsBashforth-step-4 ">>> def f(x,y):\n...     return x + y\n>>> y = AdamsBashforth(\n...    f, [0, 0.2, 0.4, 0.6], [0, 0, 0.04, 0.128], 0.2, 1).step_4()\n>>> float(y[4])\n0.30699999999999994\n>>> float(y[5])\n0.5771083333333333\n\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_4()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn AdamsBashforth-step-5 ">>> def f(x,y):\n...     return x + y\n>>> y = AdamsBashforth(\n...     f, [0, 0.2, 0.4, 0.6, 0.8], [0, 0.02140, 0.02140, 0.22211, 0.42536],\n...     0.2, 1).step_5()\n>>> float(y[-1])\n0.05436839444444452\n\n>>> AdamsBashforth(f, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1).step_5()\nTraceback (most recent call last):\n    ...\nValueError: Insufficient initial points information."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: AdamsBashforth.step_2 stubbed (np)
;; NOTE: AdamsBashforth.step_3 stubbed (np)
;; NOTE: AdamsBashforth.step_4 stubbed (np)
;; NOTE: AdamsBashforth.step_5 stubbed (np)
