(ns clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix
  "Given an matrix of numbers in which all rows and all columns are sorted in decreasing\norder, return the number of negative numbers in grid.\n\nReference: https://leetcode.com/problems/count-negative-numbers-in-a-sorted-matrix"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare generate-large-matrix validate-grid find-negative-index count-negatives-binary-search count-negatives-brute-force count-negatives-brute-force-with-break benchmark grid test-grids)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice py-add py-sub py-repeat-vec py-mul py-floordiv py-neg py-sorted py-list py-float-str py-quote py-repr py-str)

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

(defn generate-large-matrix ">>> generate_large_matrix() # doctest: +ELLIPSIS\n[[1000, ..., -999], [999, ..., -1001], ..., [2, ..., -1998]]"
  [] (vec (for [i (py-iter (range 1000))] (py-list (range (py-sub 1000 i) (py-sub (py-neg 1000) i) (py-neg 1))))))

(defn validate-grid "Validate that the rows and columns of the grid is sorted in decreasing order.\n>>> for grid in test_grids:\n...     validate_grid(grid)"
  [grid] (assert (py-truthy (every? py-truthy (doall (for [row (py-iter grid)] (py-eq row (if (py-truthy true) (vec (reverse (py-sorted row))) (py-sorted row)))))))) (assert (py-truthy (every? py-truthy (doall (for [col (py-iter (apply map vector grid))] (py-eq (py-list col) (if (py-truthy true) (vec (reverse (py-sorted col))) (py-sorted col)))))))) nil)

(defn find-negative-index "Find the smallest negative index\n\n>>> find_negative_index([0,0,0,0])\n4\n>>> find_negative_index([4,3,2,-1])\n3\n>>> find_negative_index([1,0,-1,-10])\n2\n>>> find_negative_index([0,0,0,-1])\n3\n>>> find_negative_index([11,8,7,-3,-5,-9])\n3\n>>> find_negative_index([-1,-1,-2,-3])\n0\n>>> find_negative_index([5,1,0])\n3\n>>> find_negative_index([-5,-5,-5])\n0\n>>> find_negative_index([0])\n1\n>>> find_negative_index([])\n0"
  [array] (let [left (atom nil) mid (atom nil) num (atom nil) right (atom nil)] (try (reset! left 0) (reset! right (py-sub (count array) 1)) (when (py-truthy (let [__b1 (not (py-truthy array))] (if (py-truthy __b1) __b1 (py-lt (py-get array 0) 0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (while (py-truthy (py-gt (py-add @right 1) @left)) (reset! mid (py-floordiv (py-add @left @right) 2)) (reset! num (py-get array @mid)) (when (py-truthy (let [__b2 (py-lt @num 0)] (if (py-truthy __b2) (py-ge (py-get array (py-sub @mid 1)) 0) __b2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @mid}))) (if (py-truthy (py-ge @num 0)) (do (reset! left (py-add @mid 1))) (do (reset! right (py-sub @mid 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (count array)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn count-negatives-binary-search "An O(m logn) solution that uses binary search in order to find the boundary between\npositive and negative numbers\n\n>>> [count_negatives_binary_search(grid) for grid in test_grids]\n[8, 0, 0, 3, 1498500]"
  [grid] (let [bound (atom nil) i (atom nil) total (atom nil)] (try (reset! total 0) (reset! bound (count (py-get grid 0))) (doseq [__i1 (py-iter (range (count grid)))] (reset! i __i1) (reset! bound (find-negative-index (py-slice (py-get grid @i) nil @bound nil))) (swap! total py-add @bound)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sub (py-mul (count grid) (count (py-get grid 0))) @total)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn count-negatives-brute-force "This solution is O(n^2) because it iterates through every column and row.\n\n>>> [count_negatives_brute_force(grid) for grid in test_grids]\n[8, 0, 0, 3, 1498500]"
  [grid] (count (vec (for [row (py-iter grid) number (py-iter row) :when (py-truthy (py-lt number 0))] number))))

(defn count-negatives-brute-force-with-break "Similar to the brute force solution above but uses break in order to reduce the\nnumber of iterations.\n\n>>> [count_negatives_brute_force_with_break(grid) for grid in test_grids]\n[8, 0, 0, 3, 1498500]"
  [grid] (let [i (atom nil) number (atom nil) row (atom nil) total (atom nil)] (try (reset! total 0) (doseq [__i1 (py-iter grid)] (reset! row __i1) (try (doseq [[__i2 __i3] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @row))] (reset! i __i2) (reset! number __i3) (when (py-truthy (py-lt @number 0)) (swap! total py-add (py-sub (count @row) @i)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @total})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn benchmark "Benchmark our functions next to each other"
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

(def grid (generate-large-matrix))

(def test-grids [[[4 3 2 (py-neg 1)] [3 2 1 (py-neg 1)] [1 1 (py-neg 1) (py-neg 2)] [(py-neg 1) (py-neg 1) (py-neg 2) (py-neg 3)]] [[3 2] [1 0]] [[7 7 6]] [[7 7 6] [(py-neg 1) (py-neg 2) (py-neg 3)]] grid])

;; NOTE: benchmark stubbed (timeit)
