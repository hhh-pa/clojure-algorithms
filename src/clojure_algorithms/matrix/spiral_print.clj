(ns clojure-algorithms.matrix.spiral-print
  "This program print the matrix in spiral form.\nThis problem has been solved through recursive way.\n      Matrix must satisfy below conditions\n        i) matrix should be only one or two dimensional\n        ii) number of column of all rows should be equal"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare check-matrix spiral-print-clockwise spiral-traversal)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice py-add py-sub py-neg py-list py-pop py-float-str py-quote py-repr py-str)

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-pop
  ([c k] (py-pop c k :py-no-default))
  ([c k dflt]
   (cond (map? c) (if (contains? c k) (get c k)
                      (if (= dflt :py-no-default)
                        (throw (ex-info (str "key: " k) {:type :KeyError})) dflt))
         (string? c) (throw (ex-info "no attribute 'pop'" {:type :AttributeError}))
         (set? c) (if (nil? k)
                    (if (empty? c)
                      (throw (ex-info "pop from empty set" {:type :KeyError}))
                      (first c))
                    (if (contains? c k) k
                        (throw (ex-info (str "key: " k) {:type :KeyError}))))
         :else (let [v (vec c)]
                 (if (nil? k)
                   (if (empty? v)
                     (throw (ex-info "pop from empty list" {:type :IndexError}))
                     (peek v))
                   (nth v (py-index-norm k (count v))))))))

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

(defn check-matrix
  [matrix] (let [matrix (atom matrix)] (let [prev-len (atom nil) result (atom nil) row (atom nil)] (try (reset! matrix (vec (for [row (py-iter @matrix)] (py-list row)))) (if (py-truthy (let [__b1 @matrix] (if (py-truthy __b1) (sequential? @matrix) __b1))) (do (if (py-truthy (sequential? (py-get @matrix 0))) (do (reset! prev-len 0) (doseq [__i2 (py-iter @matrix)] (reset! row __i2) (if (py-truthy (py-eq @prev-len 0)) (do (reset! prev-len (count @row)) (reset! result true)) (do (reset! result (py-eq @prev-len (count @row))))))) (do (reset! result true)))) (do (reset! result false))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn spiral-print-clockwise ">>> spiral_print_clockwise([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]])\n1\n2\n3\n4\n8\n12\n11\n10\n9\n5\n6\n7"
  [a] (let [a (atom a)] (let [dat (atom nil) i (atom nil) mat-col (atom nil) mat-row (atom nil) remain-mat (atom nil)] (try (if (py-truthy (let [__b1 (check-matrix @a)] (if (py-truthy __b1) (py-gt (count @a) 0) __b1))) (do (reset! a (vec (for [row (py-iter @a)] (py-list row)))) (reset! mat-row (count @a)) (if (py-truthy (sequential? (py-get @a 0))) (do (reset! mat-col (count (py-get @a 0)))) (do (doseq [__i2 (py-iter @a)] (reset! dat __i2) (println (clojure.string/join " " [(py-str @dat)]))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil})))) (doseq [__i3 (py-iter (range @mat-col))] (reset! i __i3) (println (clojure.string/join " " [(py-str (py-get (py-get @a 0) @i))]))) (doseq [__i4 (py-iter (range 1 @mat-row))] (reset! i __i4) (println (clojure.string/join " " [(py-str (py-get (py-get @a @i) (py-sub @mat-col 1)))]))) (when (py-truthy (py-gt @mat-row 1)) (doseq [__i5 (py-iter (range (py-sub @mat-col 2) (py-neg 1) (py-neg 1)))] (reset! i __i5) (println (clojure.string/join " " [(py-str (py-get (py-get @a (py-sub @mat-row 1)) @i))])))) (doseq [__i6 (py-iter (range (py-sub @mat-row 2) 0 (py-neg 1)))] (reset! i __i6) (println (clojure.string/join " " [(py-str (py-get (py-get @a @i) 0))]))) (reset! remain-mat (vec (for [row (py-iter (py-slice @a 1 (py-sub @mat-row 1) nil))] (py-slice row 1 (py-sub @mat-col 1) nil)))) (if (py-truthy (py-gt (count @remain-mat) 0)) (do (spiral-print-clockwise @remain-mat)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))))) (do (println (clojure.string/join " " [(py-str "Not a valid matrix")])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn spiral-traversal ">>> spiral_traversal([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]])\n[1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7]\n\nExample:\nmatrix = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]\nAlgorithm:\n    Step 1. first pop the 0 index list. (which is [1,2,3,4] and concatenate the\n            output of [step 2])\n    Step 2. Now perform matrix's Transpose operation (Change rows to column\n            and vice versa) and reverse the resultant matrix.\n    Step 3. Pass the output of [2nd step], to same recursive function till\n            base case hits.\nDry Run:\nStage 1.\n[1, 2, 3, 4] +   spiral_traversal([\n    [8, 12], [7, 11], [6, 10], [5, 9]]\n ])\nStage 2.\n[1, 2, 3, 4, 8, 12] + spiral_traversal([\n    [11, 10, 9], [7, 6, 5]\n])\nStage 3.\n[1, 2, 3, 4, 8, 12, 11, 10, 9] + spiral_traversal([\n    [5], [6], [7]\n])\nStage 4.\n[1, 2, 3, 4, 8, 12, 11, 10, 9, 5] + spiral_traversal([\n    [5], [6], [7]\n])\nStage 5.\n[1, 2, 3, 4, 8, 12, 11, 10, 9, 5] + spiral_traversal([[6, 7]])\nStage 6.\n[1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7] + spiral_traversal([])"
  [matrix] (let [matrix (atom matrix)] (try (if (py-truthy @matrix) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-list (py-pop @matrix 0)) (spiral-traversal (py-slice (vec (for [row (py-iter (apply map vector @matrix))] (py-list row))) nil nil (py-neg 1))))}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value []})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
