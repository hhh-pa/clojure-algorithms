(ns clojure-algorithms.matrix.rotate-matrix
  "In this problem, we want to rotate the matrix elements by 90, 180, 270\n(counterclockwise)\nDiscussion in stackoverflow:\nhttps://stackoverflow.com/questions/42519/how-do-you-rotate-a-two-dimensional-array"
  (:require [clojure.string :as str]))

(declare make-matrix rotate-90 rotate-180 rotate-270 transpose reverse-row reverse-column print-matrix)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-slice py-add py-repeat-vec py-mul py-neg py-abs py-list py-float-str py-quote py-repr py-str)

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

(defn make-matrix ">>> make_matrix()\n[[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]\n>>> make_matrix(1)\n[[1]]\n>>> make_matrix(-2)\n[[1, 2], [3, 4]]\n>>> make_matrix(3)\n[[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n>>> make_matrix() == make_matrix(4)\nTrue"
  ([] (make-matrix 4))
  ([row-size] (let [row-size (atom row-size)] (try (reset! row-size (let [__b1 (py-abs @row-size)] (if (py-truthy __b1) __b1 4))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [y (py-iter (range @row-size))] (vec (for [x (py-iter (range @row-size))] (py-add (py-add 1 x) (py-mul y @row-size))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn rotate-90 ">>> rotate_90(make_matrix())\n[[4, 8, 12, 16], [3, 7, 11, 15], [2, 6, 10, 14], [1, 5, 9, 13]]\n>>> rotate_90(make_matrix()) == transpose(reverse_column(make_matrix()))\nTrue"
  [matrix] (reverse-row (transpose matrix)))

(defn rotate-180 ">>> rotate_180(make_matrix())\n[[16, 15, 14, 13], [12, 11, 10, 9], [8, 7, 6, 5], [4, 3, 2, 1]]\n>>> rotate_180(make_matrix()) == reverse_column(reverse_row(make_matrix()))\nTrue"
  [matrix] (reverse-row (reverse-column matrix)))

(defn rotate-270 ">>> rotate_270(make_matrix())\n[[13, 9, 5, 1], [14, 10, 6, 2], [15, 11, 7, 3], [16, 12, 8, 4]]\n>>> rotate_270(make_matrix()) == transpose(reverse_row(make_matrix()))\nTrue"
  [matrix] (reverse-column (transpose matrix)))

(defn transpose
  [matrix] (let [matrix (atom matrix)] (try (swap! matrix (fn [__c] (py-assoc-in __c [[:slice nil nil nil]] (vec (for [x (py-iter (apply map vector @matrix))] (py-list x)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn reverse-row
  [matrix] (let [matrix (atom matrix)] (try (swap! matrix (fn [__c] (py-assoc-in __c [[:slice nil nil nil]] (py-slice @matrix nil nil (py-neg 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn reverse-column
  [matrix] (let [matrix (atom matrix)] (try (swap! matrix (fn [__c] (py-assoc-in __c [[:slice nil nil nil]] (vec (for [x (py-iter @matrix)] (py-slice x nil nil (py-neg 1))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn print-matrix
  [matrix] (let [i (atom nil)] (doseq [__i1 (py-iter matrix)] (reset! i __i1) (print (str (clojure.string/join " " (concat (map py-str (seq @i)))) "\n"))) nil))
