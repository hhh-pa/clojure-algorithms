(ns clojure-algorithms.maths.gcd-of-n-numbers
  "Gcd of N Numbers\nReference: https://en.wikipedia.org/wiki/Greatest_common_divisor"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare get-factors get-greatest-common-divisor)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-add py-repeat-vec py-mul py-floordiv py-mod py-pow py-band py-counter py-float-str py-quote py-repr py-str)

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

(defn- py-floordiv [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :floordiv a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for //" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :floordiv-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for //" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for //" {:type :TypeError})))
                (when (zero? b)
                  (throw (ex-info "integer division by zero" {:type :ZeroDivisionError})))
                (let [r (/ (rationalize a) (rationalize b))]
                  (if (integer? r)
                    (if (or (float? a) (float? b)) (double r) r)
                    (let [n (numerator r) d (denominator r)
                          q (quot n d)
                          qq (if (and (not (zero? (rem n d))) (neg? n)) (dec q) q)]
                      (if (or (float? a) (float? b)) (double qq) qq)))))))

(defn- py-mod [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :mod a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for %" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :mod-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for %" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for %" {:type :TypeError})))
                (when (zero? b)
                  (throw (ex-info "integer modulo by zero" {:type :ZeroDivisionError})))
                (mod a b))))

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

(defn- py-counter [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (string? x) (frequencies (map str (seq x)))
        :else (frequencies (py-iter x))))

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

(defn get-factors "this is a recursive function for get all factors of number\n>>> get_factors(45)\nCounter({3: 2, 5: 1})\n>>> get_factors(2520)\nCounter({2: 3, 3: 2, 5: 1, 7: 1})\n>>> get_factors(23)\nCounter({23: 1})\n>>> get_factors(0)\nTraceback (most recent call last):\n    ...\nTypeError: number must be integer and greater than zero\n>>> get_factors(-1)\nTraceback (most recent call last):\n    ...\nTypeError: number must be integer and greater than zero\n>>> get_factors(1.5)\nTraceback (most recent call last):\n    ...\nTypeError: number must be integer and greater than zero\n\nfactor can be all numbers from 2 to number that we check if number % factor == 0\nif it is equal to zero, we check again with number // factor\nelse we increase factor by one"
  ([number] (get-factors number nil 2))
  ([number factors] (get-factors number factors 2))
  ([number factors factor] (let [number (atom number) factors (atom factors)] (let [num (atom nil)] (try (reset! number (if (py-truthy (integer? @number)) @number nil)) (reset! num (if (py-truthy (integer? @number)) @number nil)) (if (py-truthy (let [__b1 (integer? @number)] (if (py-truthy __b1) (py-eq @number 1) __b1))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-counter {1 1})}))) (do (if (py-truthy (let [__b2 (integer? @number)] (if (py-truthy __b2) (py-gt @number 0) __b2))) (do (reset! number @num)) (do (throw (ex-info (py-str "number must be integer and greater than zero") {:type :TypeError})))))) (reset! factors (let [__b3 @factors] (if (py-truthy __b3) __b3 (py-counter [])))) (when (py-truthy (py-eq @number factor)) (swap! factors (fn [__c] (py-assoc-in __c [factor] (py-add (py-get-in __c [factor]) 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @factors}))) (when (py-truthy (py-gt (py-mod @number factor) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (get-factors @number @factors (py-add factor 1))}))) (swap! factors (fn [__c] (py-assoc-in __c [factor] (py-add (py-get-in __c [factor]) 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (get-factors (py-floordiv @number factor) @factors factor)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))))

(defn get-greatest-common-divisor "get gcd of n numbers:\n>>> get_greatest_common_divisor(18, 45)\n9\n>>> get_greatest_common_divisor(23, 37)\n1\n>>> get_greatest_common_divisor(2520, 8350)\n10\n>>> get_greatest_common_divisor(-10, 20)\nTraceback (most recent call last):\n    ...\nException: numbers must be integer and greater than zero\n>>> get_greatest_common_divisor(1.5, 2)\nTraceback (most recent call last):\n    ...\nException: numbers must be integer and greater than zero\n>>> get_greatest_common_divisor(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)\n1\n>>> get_greatest_common_divisor(\"1\", 2, 3, 4, 5, 6, 7, 8, 9, 10)\nTraceback (most recent call last):\n    ...\nException: numbers must be integer and greater than zero"
  [& numbers] (let [factor (atom nil) factors (atom nil) m (atom nil) mult (atom nil) same-factors (atom nil)] (try (try (let [__d1 (vec (map get-factors numbers))] (reset! same-factors (nth __d1 0)) (reset! factors (vec (drop 1 __d1)))) (catch clojure.lang.ExceptionInfo __eh (if (:__control__ (ex-data __eh)) (throw __eh) (let [__t (:type (ex-data __eh))] (cond (contains? #{:TypeError} __t) (do (let [e __eh] (throw (ex-info (py-str "numbers must be integer and greater than zero") {:type :Exception})))) :else (throw __eh)))))) (doseq [__i2 (py-iter @factors)] (reset! factor __i2) (swap! same-factors py-band @factor)) (reset! mult 1) (doseq [__i3 (py-iter (vec (for [[factor power] (py-iter (vec @same-factors))] (py-pow factor power))))] (reset! m __i3) (swap! mult py-mul @m)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @mult})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
