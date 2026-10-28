(ns clojure-algorithms.maths.triplet-sum
  "Given an array of integers and another integer target,\nwe are required to find a triplet from the array such that it's sum is equal to\nthe target."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare make-dataset triplet-sum1 triplet-sum2 solution-times dataset)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-neg py-sorted py-list py-permutations py-float-str py-quote py-repr py-rng py-rand py-randint)

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-sorted [c]
  (cond (string? c) (vec (sort (map str (seq c))))
        (map? c) (vec (sort (keys c)))
        :else (try (vec (sort c))
                   (catch ClassCastException e
                     (throw (ex-info "not orderable" {:type :TypeError}))))))

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-permutations [xs r]
  (let [v (vec xs) n (count v)]
    (cond (neg? r) (throw (ex-info "r must be non-negative" {:type :ValueError}))
          (zero? r) [[]]
          (> r n) []
          :else (vec (mapcat (fn [i]
                               (map #(vec (cons (nth v i) %))
                                    (py-permutations
                                     (vec (concat (subvec v 0 i)
                                                  (subvec v (inc i))))
                                     (dec r))))
                             (range n))))))

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

(def ^:private py-rng (atom (java.util.Random.)))

(defn- py-rand [] (.nextDouble ^java.util.Random @py-rng))

(defn- py-randint [a b]
  (let [a (long a) b (long b) n (inc (- b a))]
    (if (<= n 0) (throw (ex-info "empty range" {:type :ValueError}))
        (if (<= n Integer/MAX_VALUE) (+ a (.nextInt ^java.util.Random @py-rng (int n)))
            (+ a (long (* (py-rand) n)))))))

(defn make-dataset
  [] (let [arr (atom nil) r (atom nil)] (try (reset! arr (vec (for [i (py-iter (range 10))] (py-randint (py-neg 1000) 1000)))) (reset! r (py-randint (py-neg 5000) 5000)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@arr @r]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn triplet-sum1 "Returns a triplet in the array with sum equal to target,\nelse (0, 0, 0).\n>>> triplet_sum1([13, 29, 7, 23, 5], 35)\n(5, 7, 23)\n>>> triplet_sum1([37, 9, 19, 50, 44], 65)\n(9, 19, 37)\n>>> arr = [6, 47, 27, 1, 15]\n>>> target = 11\n>>> triplet_sum1(arr, target)\n(0, 0, 0)"
  [arr target] (let [triplet (atom nil)] (try (doseq [__i1 (py-iter (py-permutations arr 3))] (reset! triplet __i1) (when (py-truthy (py-eq (reduce +' 0 @triplet) target)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-list (py-sorted @triplet))})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [0 0 0]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn triplet-sum2 "Returns a triplet in the array with sum equal to target,\nelse (0, 0, 0).\n>>> triplet_sum2([13, 29, 7, 23, 5], 35)\n(5, 7, 23)\n>>> triplet_sum2([37, 9, 19, 50, 44], 65)\n(9, 19, 37)\n>>> arr = [6, 47, 27, 1, 15]\n>>> target = 11\n>>> triplet_sum2(arr, target)\n(0, 0, 0)"
  [arr target] (let [arr (atom arr)] (let [i (atom nil) left (atom nil) n (atom nil) right (atom nil)] (try (reset! arr (py-sorted @arr)) (reset! n (count @arr)) (doseq [__i1 (py-iter (range (py-sub @n 1)))] (reset! i __i1) (let [__d2 (vec [(py-add @i 1) (py-sub @n 1)])] (reset! left (nth __d2 0)) (reset! right (nth __d2 1))) (while (py-truthy (py-lt @left @right)) (if (py-truthy (py-eq (py-add (py-add (py-get @arr @i) (py-get @arr @left)) (py-get @arr @right)) target)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-get @arr @i) (py-get @arr @left) (py-get @arr @right)]}))) (do (if (py-truthy (py-lt (py-add (py-add (py-get @arr @i) (py-get @arr @left)) (py-get @arr @right)) target)) (do (swap! left py-add 1)) (do (when (py-truthy (py-gt (py-add (py-add (py-get @arr @i) (py-get @arr @left)) (py-get @arr @right)) target)) (swap! right py-sub 1)))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [0 0 0]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn solution-times
  [& _args]
  ;; stub: original requires repeat; pure-Clojure port not applicable
  (throw (ex-info "requires repeat" {:type :UnsupportedOperation})))

(def dataset (make-dataset))

;; NOTE: solution_times stubbed (repeat)
