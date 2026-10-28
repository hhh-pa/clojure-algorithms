(ns clojure-algorithms.matrix.matrix-operation
  "Functions for 2D matrix operations"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare add subtract scalar-multiply multiply identity transpose minor determinant permanent inverse -check-not-integer -shape -verify-matrix-sizes main)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice py-add py-sub py-repeat-vec py-mul py-div py-pow py-neg py-list py-float-str py-quote py-repr py-str py-int)

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

(defn- py-int [x base]
  (cond (boolean? x) (if x 1 0)
        (char? x) (py-int (str x) base)
        (integer? x) (if (nil? base) x
                         (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (ratio? x) (if (nil? base) (long x)
                       (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (decimal? x) (if (nil? base) (long x)
                         (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (float? x) (if (nil? base)
                     (try (long x)
                          (catch Exception e
                            (throw (ex-info "cannot convert float to int" {:type :OverflowError}))))
                     (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (string? x)
        (let [s (clojure.string/replace (clojure.string/trim x) "_" "")]
          (try
            (if (nil? base)
              (bigint (java.math.BigInteger. s 10))
              (let [b (long base)]
                (cond (= b 0) (cond (re-matches #"(?i)[+-]?0x[0-9a-f]+" s) (bigint (java.math.BigInteger. s 16))
                                    (re-matches #"(?i)[+-]?0o[0-7]+" s) (bigint (java.math.BigInteger. (clojure.string/replace s #"(?i)0o" "") 8))
                                    (re-matches #"(?i)[+-]?0b[01]+" s) (bigint (java.math.BigInteger. (clojure.string/replace s #"(?i)0b" "") 2))
                                    :else (bigint (java.math.BigInteger. s 10)))
                      (or (< b 2) (> b 36)) (throw (ex-info "int() base must be >= 2 and <= 36" {:type :ValueError}))
                      :else (bigint (java.math.BigInteger. s (int b))))))
            (catch clojure.lang.ExceptionInfo e (throw e))
            (catch NumberFormatException e
              (throw (ex-info (str "invalid literal for int(): " x) {:type :ValueError})))))
        :else (throw (ex-info (str "int() argument must be a string or a number") {:type :TypeError}))))

(defn add ">>> add([[1,2],[3,4]],[[2,3],[4,5]])\n[[3, 5], [7, 9]]\n>>> add([[1.2,2.4],[3,4]],[[2,3],[4,5]])\n[[3.2, 5.4], [7, 9]]\n>>> add([[1, 2], [4, 5]], [[3, 7], [3, 4]], [[3, 5], [5, 7]])\n[[7, 14], [12, 16]]\n>>> add([3], [4, 5])\nTraceback (most recent call last):\n  ...\nTypeError: Expected a matrix, got int/list instead"
  [& matrix-s] (let [i (atom nil)] (try (when (py-truthy (every? py-truthy (doall (for [m (py-iter matrix-s)] (-check-not-integer m))))) (doseq [__i1 (py-iter (py-slice matrix-s 1 nil nil))] (reset! i __i1) (-verify-matrix-sizes (py-get matrix-s 0) @i)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [m (py-iter (apply map vector matrix-s))] (vec (for [t (py-iter (apply map vector m))] (reduce +' 0 t)))))}))) (throw (ex-info (py-str "Expected a matrix, got int/list instead") {:type :TypeError})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn subtract ">>> subtract([[1,2],[3,4]],[[2,3],[4,5]])\n[[-1, -1], [-1, -1]]\n>>> subtract([[1,2.5],[3,4]],[[2,3],[4,5.5]])\n[[-1, -0.5], [-1, -1.5]]\n>>> subtract([3], [4, 5])\nTraceback (most recent call last):\n  ...\nTypeError: Expected a matrix, got int/list instead"
  [matrix-a matrix-b] (try (when (py-truthy (let [__b2 (-check-not-integer matrix-a)] (if (py-truthy __b2) (let [__b1 (-check-not-integer matrix-b)] (if (py-truthy __b1) (-verify-matrix-sizes matrix-a matrix-b) __b1)) __b2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [m (py-iter (map vector matrix-a matrix-b))] (vec (for [[i j] (py-iter (apply map vector m))] (py-sub i j)))))}))) (throw (ex-info (py-str "Expected a matrix, got int/list instead") {:type :TypeError})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn scalar-multiply ">>> scalar_multiply([[1,2],[3,4]],5)\n[[5, 10], [15, 20]]\n>>> scalar_multiply([[1.4,2.3],[3,4]],5)\n[[7.0, 11.5], [15, 20]]"
  [matrix n] (vec (for [row (py-iter matrix)] (vec (for [x (py-iter row)] (py-mul x n))))))

(defn multiply ">>> multiply([[1,2],[3,4]],[[5,5],[7,5]])\n[[19, 15], [43, 35]]\n>>> multiply([[1,2.5],[3,4.5]],[[5,5],[7,5]])\n[[22.5, 17.5], [46.5, 37.5]]\n>>> multiply([[1, 2, 3]], [[2], [3], [4]])\n[[20]]"
  [matrix-a matrix-b] (let [cols (atom nil) msg (atom nil) rows (atom nil)] (try (when (py-truthy (let [__b1 (-check-not-integer matrix-a)] (if (py-truthy __b1) (-check-not-integer matrix-b) __b1))) (let [__d2 (vec (-verify-matrix-sizes matrix-a matrix-b))] (reset! rows (nth __d2 0)) (reset! cols (nth __d2 1)))) (when (py-truthy (not (py-eq (py-get @cols 0) (py-get @rows 1)))) (reset! msg (str "Cannot multiply matrix of dimensions (" (py-str (py-get @rows 0)) "," (py-str (py-get @cols 0)) ") and (" (py-str (py-get @rows 1)) "," (py-str (py-get @cols 1)) ")")) (throw (ex-info (py-str @msg) {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter matrix-a)] (vec (for [j (py-iter (apply map vector matrix-b))] (reduce +' 0 (doall (for [[m n] (py-iter (map vector i j))] (py-mul m n))))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn identity ":param n: dimension for nxn matrix\n:type n: int\n:return: Identity matrix of shape [n, n]\n>>> identity(3)\n[[1, 0, 0], [0, 1, 0], [0, 0, 1]]"
  [n] (let [n (atom n)] (try (reset! n (py-int @n nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [row (py-iter (range @n))] (vec (for [column (py-iter (range @n))] (py-int (py-eq row column) nil)))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn transpose ">>> transpose([[1,2],[3,4]]) # doctest: +ELLIPSIS\n<map object at ...\n>>> transpose([[1,2],[3,4]], return_map=False)\n[[1, 3], [2, 4]]\n>>> transpose([1, [2, 3]])\nTraceback (most recent call last):\n  ...\nTypeError: Expected a matrix, got int/list instead"
  ([matrix] (transpose matrix true))
  ([matrix return-map] (try (when (py-truthy (-check-not-integer matrix)) (if (py-truthy return-map) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (map py-list (apply map vector matrix))}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-list (map py-list (apply map vector matrix)))}))))) (throw (ex-info (py-str "Expected a matrix, got int/list instead") {:type :TypeError})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn minor ">>> minor([[1, 2], [3, 4]], 1, 1)\n[[1]]"
  [matrix row column] (let [minor (atom nil)] (try (reset! minor (py-add (py-slice matrix nil row nil) (py-slice matrix (py-add row 1) nil nil))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [row (py-iter @minor)] (py-add (py-slice row nil column nil) (py-slice row (py-add column 1) nil nil))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn determinant ">>> determinant([[1, 2], [3, 4]])\n-2\n>>> determinant([[1.5, 2.5], [3, 4]])\n-1.5"
  [matrix] (try (when (py-truthy (py-eq (count matrix) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get matrix 0) 0)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [[i x] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-get matrix 0)))] (py-mul (py-mul x (determinant (minor matrix 0 i))) (py-pow (py-neg 1) i)))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn permanent ">>> permanent([[1, 2], [3, 4]])\n10\n>>> permanent([[1.5, 2.5], [3, 4]])\n13.5"
  [matrix] (try (when (py-truthy (py-eq (count matrix) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get matrix 0) 0)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [[i x] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-get matrix 0)))] (py-mul x (permanent (minor matrix 0 i))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn inverse ">>> inverse([[1, 2], [3, 4]])\n[[-2.0, 1.0], [1.5, -0.5]]\n>>> inverse([[1, 1], [1, 1]])"
  [matrix] (let [adjugate (atom nil) cofactors (atom nil) det (atom nil) matrix-minor (atom nil)] (try (reset! det (determinant matrix)) (when (py-truthy (py-eq @det 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (reset! matrix-minor (vec (for [i (py-iter (range (count matrix)))] (vec (for [j (py-iter (range (count matrix)))] (determinant (minor matrix i j))))))) (reset! cofactors (vec (for [row (py-iter (range (count matrix)))] (vec (for [[col x] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-get @matrix-minor row)))] (py-mul x (py-pow (py-neg 1) (py-add row col)))))))) (reset! adjugate (py-list (transpose @cofactors))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (scalar-multiply @adjugate (py-div 1 @det))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn -check-not-integer
  [matrix] (let [__b1 (not (py-truthy (integer? matrix)))] (if (py-truthy __b1) (not (py-truthy (integer? (py-get matrix 0)))) __b1)))

(defn -shape
  [matrix] [(count matrix) (count (py-get matrix 0))])

(defn -verify-matrix-sizes
  [matrix-a matrix-b] (let [msg (atom nil) shape (atom nil)] (try (reset! shape (py-add (-shape matrix-a) (-shape matrix-b))) (when (py-truthy (let [__b1 (not (py-eq (py-get @shape 0) (py-get @shape 3)))] (if (py-truthy __b1) __b1 (not (py-eq (py-get @shape 1) (py-get @shape 2)))))) (reset! msg (str "operands could not be broadcast together with shape (" (py-str [(py-get @shape 0) (py-get @shape 1)]) "), (" (py-str [(py-get @shape 2) (py-get @shape 3)]) ")")) (throw (ex-info (py-str @msg) {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [[(py-get @shape 0) (py-get @shape 2)] [(py-get @shape 1) (py-get @shape 3)]]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [] (let [matrix-a (atom nil) matrix-b (atom nil) matrix-c (atom nil) matrix-d (atom nil)] (reset! matrix-a [[12 10] [3 9]]) (reset! matrix-b [[3 4] [7 4]]) (reset! matrix-c [[11 12 13 14] [21 22 23 24] [31 32 33 34] [41 42 43 44]]) (reset! matrix-d [[3 0 2] [2 0 (py-neg 2)] [0 1 1]]) (println (clojure.string/join " " [(py-str (str "Add Operation, add(matrix_a, matrix_b) = " (py-str (pr-str (add @matrix-a @matrix-b))) " \n"))])) (println (clojure.string/join " " [(py-str (str "Multiply Operation, multiply(matrix_a, matrix_b) = " (py-str (pr-str (multiply @matrix-a @matrix-b))) " \n"))])) (println (clojure.string/join " " [(py-str (str "Identity: " (py-str (identity 5)) "\n"))])) (println (clojure.string/join " " [(py-str (str "Minor of " (py-str @matrix-c) " = " (py-str (minor @matrix-c 1 2)) " \n"))])) (println (clojure.string/join " " [(py-str (str "Determinant of " (py-str @matrix-b) " = " (py-str (determinant @matrix-b)) " \n"))])) (println (clojure.string/join " " [(py-str (str "Permanent of " (py-str @matrix-b) " = " (py-str (permanent @matrix-b)) " \n"))])) (println (clojure.string/join " " [(py-str (str "Inverse of " (py-str @matrix-d) " = " (py-str (inverse @matrix-d)) "\n"))])) nil))
