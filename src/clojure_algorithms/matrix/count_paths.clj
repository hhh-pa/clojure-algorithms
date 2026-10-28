(ns clojure-algorithms.matrix.count-paths
  "Given a grid, where you start from the top left position [0, 0],\nyou want to find how many paths you can take to get to the bottom right position.\n\nstart here  ->   0  0  0  0\n                 1  1  0  0\n                 0  0  0  1\n                 0  1  0  0  <- finish here\nhow many 'distinct' paths can you take to get to the finish?\nUsing a recursive depth-first search algorithm below, you are able to\nfind the number of distinct unique paths (count).\n\n'*' will demonstrate a path\nIn the example above, there are two distinct paths:\n1.                2.\n    *  *  *  0      *  *  *  *\n    1  1  *  0      1  1  *  *\n    0  0  *  1      0  0  *  1\n    0  1  *  *      0  1  *  *"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare depth-first-search)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-min py-min-by py-max py-max-by py-remove-first py-float-str py-quote py-repr)

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

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-remove-first [c x]
  (cond (set? c) (if (contains? c x) (disj c x)
                     (throw (ex-info (str "key: " x) {:type :KeyError})))
        (sequential? c) (let [v (vec c)
                              i (first (keep-indexed (fn [j e] (when (py-eq e x) j)) v))]
                          (if (nil? i)
                            (throw (ex-info "list.remove(x): x not in list" {:type :ValueError}))
                            (vec (concat (subvec v 0 i) (subvec v (inc i))))))
        :else (throw (ex-info "no attribute 'remove'" {:type :AttributeError}))))

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

(defn depth-first-search "Recursive Backtracking Depth First Search Algorithm\n\nStarting from top left of a matrix, count the number of\npaths that can reach the bottom right of a matrix.\n1 represents a block (inaccessible)\n0 represents a valid space (accessible)\n\n0  0  0  0\n1  1  0  0\n0  0  0  1\n0  1  0  0\n>>> grid = [[0, 0, 0, 0], [1, 1, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]]\n>>> depth_first_search(grid, 0, 0, set())\n2\n\n0  0  0  0  0\n0  1  1  1  0\n0  1  1  1  0\n0  0  0  0  0\n>>> grid = [[0, 0, 0, 0, 0], [0, 1, 1, 1, 0], [0, 1, 1, 1, 0], [0, 0, 0, 0, 0]]\n>>> depth_first_search(grid, 0, 0, set())\n2"
  [grid row col visit] (let [visit (atom visit)] (let [col-length (atom nil) count-v (atom nil) row-length (atom nil)] (try (let [__d1 (vec [(count grid) (count (py-get grid 0))])] (reset! row-length (nth __d1 0)) (reset! col-length (nth __d1 1))) (when (py-truthy (let [__b5 (py-lt (py-min [row col]) 0)] (if (py-truthy __b5) __b5 (let [__b4 (py-eq row @row-length)] (if (py-truthy __b4) __b4 (let [__b3 (py-eq col @col-length)] (if (py-truthy __b3) __b3 (let [__b2 (py-in [row col] @visit)] (if (py-truthy __b2) __b2 (py-eq (py-get (py-get grid row) col) 1)))))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (when (py-truthy (let [__b6 (py-eq row (py-sub @row-length 1))] (if (py-truthy __b6) (py-eq col (py-sub @col-length 1)) __b6))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (swap! visit (fn [__c] (conj (set __c) [row col]))) (reset! count-v 0) (swap! count-v py-add (depth-first-search grid (py-add row 1) col @visit)) (swap! count-v py-add (depth-first-search grid (py-sub row 1) col @visit)) (swap! count-v py-add (depth-first-search grid row (py-add col 1) @visit)) (swap! count-v py-add (depth-first-search grid row (py-sub col 1) @visit)) (swap! visit py-remove-first [row col]) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @count-v})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
