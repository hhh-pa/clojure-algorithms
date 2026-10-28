(ns clojure-algorithms.matrix.matrix-class
  "Port of matrix/matrix_class.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare make-Matrix Matrix-columns Matrix-num-rows Matrix-num-columns Matrix-order Matrix-is-square Matrix-identity Matrix-determinant Matrix-is-invertable Matrix-get-minor Matrix-get-cofactor Matrix-minors Matrix-cofactors Matrix-adjugate Matrix-inverse Matrix-repr Matrix-str Matrix-add-row Matrix-add-column Matrix-eq Matrix-ne Matrix-neg Matrix-add Matrix-sub Matrix-mul Matrix-pow Matrix-dot-product)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-slice py-add py-sub py-repeat-vec py-mul py-div py-mod py-pow py-neg py-float-str py-quote py-repr py-str py-int)

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

(defrecord Matrix [rows])

(defn make-Matrix "Constructor for Matrix. Matrix object generated from a 2D array where each element is an array representing\na row.\nRows can contain type int or float.\nCommon operations and information available.\n>>> rows = [\n...     [1, 2, 3],\n...     [4, 5, 6],\n...     [7, 8, 9]\n... ]\n>>> matrix = Matrix(rows)\n>>> print(matrix)\n[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]]\n\nMatrix rows and columns are available as 2D arrays\n>>> matrix.rows\n[[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n>>> matrix.columns()\n[[1, 4, 7], [2, 5, 8], [3, 6, 9]]\n\nOrder is returned as a tuple\n>>> matrix.order\n(3, 3)\n\nSquareness and invertability are represented as bool\n>>> matrix.is_square\nTrue\n>>> matrix.is_invertable()\nFalse\n\nIdentity, Minors, Cofactors and Adjugate are returned as Matrices.  Inverse can be\na Matrix or Nonetype\n>>> print(matrix.identity())\n[[1. 0. 0.]\n [0. 1. 0.]\n [0. 0. 1.]]\n>>> print(matrix.minors())\n[[-3. -6. -3.]\n [-6. -12. -6.]\n [-3. -6. -3.]]\n>>> print(matrix.cofactors())\n[[-3. 6. -3.]\n [6. -12. 6.]\n [-3. 6. -3.]]\n>>>  # won't be apparent due to the nature of the cofactor matrix\n>>> print(matrix.adjugate())\n[[-3. 6. -3.]\n [6. -12. 6.]\n [-3. 6. -3.]]\n>>> matrix.inverse()\nTraceback (most recent call last):\n    ...\nTypeError: Only matrices with a non-zero determinant have an inverse\n\nDeterminant is an int, float, or Nonetype\n>>> matrix.determinant()\n0\n\nNegation, scalar multiplication, addition, subtraction, multiplication and\nexponentiation are available and all return a Matrix\n>>> print(-matrix)\n[[-1. -2. -3.]\n [-4. -5. -6.]\n [-7. -8. -9.]]\n>>> matrix2 = matrix * 3\n>>> print(matrix2)\n[[3. 6. 9.]\n [12. 15. 18.]\n [21. 24. 27.]]\n>>> print(matrix + matrix2)\n[[4. 8. 12.]\n [16. 20. 24.]\n [28. 32. 36.]]\n>>> print(matrix - matrix2)\n[[-2. -4. -6.]\n [-8. -10. -12.]\n [-14. -16. -18.]]\n>>> print(matrix ** 3)\n[[468. 576. 684.]\n [1062. 1305. 1548.]\n [1656. 2034. 2412.]]\n\nMatrices can also be modified\n>>> matrix.add_row([10, 11, 12])\n>>> print(matrix)\n[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]\n [10. 11. 12.]]\n>>> matrix2.add_column([8, 16, 32])\n>>> print(matrix2)\n[[3. 6. 9. 8.]\n [12. 15. 18. 16.]\n [21. 24. 27. 32.]]\n>>> print(matrix *  matrix2)\n[[90. 108. 126. 136.]\n [198. 243. 288. 304.]\n [306. 378. 450. 472.]\n [414. 513. 612. 640.]]"
  [rows] (let [self (atom {}) cols (atom nil) error (atom nil) row (atom nil) value (atom nil)] (try (reset! error (ex-info (py-str "Matrices must be formed from a list of zero or more lists containing at least one and the same number of values, each of which must be of type int or float.") {:type :TypeError})) (if (py-truthy (not (py-eq (count rows) 0))) (do (reset! cols (count (py-get rows 0))) (when (py-truthy (py-eq @cols 0)) (throw @error)) (doseq [__i1 (py-iter rows)] (reset! row __i1) (when (py-truthy (not (py-eq (count @row) @cols))) (throw @error)) (doseq [__i2 (py-iter @row)] (reset! value __i2) (when (py-truthy (not (py-truthy (or (integer? @value) (number? @value))))) (throw @error)))) (swap! self (fn [__c] (py-assoc-in __c [:rows] rows)))) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] []))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Matrix @self)))

(defn Matrix-columns
  [self] (vec (for [i (py-iter (range (count (py-get (:rows @self) 0))))] (vec (for [row (py-iter (:rows @self))] (py-get row i))))))

(defn Matrix-num-rows
  [self] (count (:rows @self)))

(defn Matrix-num-columns
  [self] (count (py-get (:rows @self) 0)))

(defn Matrix-order
  [self] [(Matrix-num-rows self) (Matrix-num-columns self)])

(defn Matrix-is-square
  [self] (py-eq (py-get (Matrix-order self) 0) (py-get (Matrix-order self) 1)))

(defn Matrix-identity
  [self] (let [values (atom nil)] (try (reset! values (vec (for [row-num (py-iter (range (Matrix-num-rows self)))] (vec (for [column-num (py-iter (range (Matrix-num-rows self)))] (if (py-truthy (not (py-eq column-num row-num))) 0 1)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix @values)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-determinant
  [self] (try (when (py-truthy (not (py-truthy (Matrix-is-square self)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (when (py-truthy (py-eq (Matrix-order self) [0 0])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (when (py-truthy (py-eq (Matrix-order self) [1 1])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-int (py-get (py-get (:rows @self) 0) 0) nil)}))) (if (py-truthy (py-eq (Matrix-order self) [2 2])) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-int (py-sub (py-mul (py-get (py-get (:rows @self) 0) 0) (py-get (py-get (:rows @self) 1) 1)) (py-mul (py-get (py-get (:rows @self) 0) 1) (py-get (py-get (:rows @self) 1) 0))) nil)}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [column (py-iter (range (Matrix-num-columns self)))] (py-mul (py-get (py-get (:rows @self) 0) column) (py-get (py-get (:rows (Matrix-cofactors self)) 0) column)))))})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-is-invertable
  [self] (boolean (py-truthy (Matrix-determinant self))))

(defn Matrix-get-minor
  [self row column] (let [values (atom nil)] (try (reset! values (vec (for [other-row (py-iter (range (Matrix-num-rows self))) :when (py-truthy (not (py-eq other-row row)))] (vec (for [other-column (py-iter (range (Matrix-num-columns self))) :when (py-truthy (not (py-eq other-column column)))] (py-get (py-get (:rows @self) other-row) other-column)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Matrix-determinant (atom (make-Matrix @values)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-get-cofactor
  [self row column] (try (when (py-truthy (py-eq (py-mod (py-add row column) 2) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Matrix-get-minor self row column)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-neg 1) (Matrix-get-minor self row column))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-minors
  [self] (make-Matrix (vec (for [row (py-iter (range (Matrix-num-rows self)))] (vec (for [column (py-iter (range (Matrix-num-columns self)))] (Matrix-get-minor self row column)))))))

(defn Matrix-cofactors
  [self] (make-Matrix (vec (for [row (py-iter (range (Matrix-num-rows (atom (Matrix-minors self)))))] (vec (for [column (py-iter (range (Matrix-num-columns (atom (Matrix-minors self)))))] (if (py-truthy (py-eq (py-mod (py-add row column) 2) 0)) (py-get (py-get (:rows (Matrix-minors self)) row) column) (py-mul (py-get (py-get (:rows (Matrix-minors self)) row) column) (py-neg 1)))))))))

(defn Matrix-adjugate
  [self] (let [values (atom nil)] (try (reset! values (vec (for [row (py-iter (range (Matrix-num-rows self)))] (vec (for [column (py-iter (range (Matrix-num-columns self)))] (py-get (py-get (:rows (Matrix-cofactors self)) column) row)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix @values)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-inverse
  [self] (let [determinant (atom nil)] (try (reset! determinant (Matrix-determinant self)) (when (py-truthy (not (py-truthy @determinant))) (throw (ex-info (py-str "Only matrices with a non-zero determinant have an inverse") {:type :TypeError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (Matrix-adjugate self) (py-div 1 @determinant))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-repr
  [self] (py-str (:rows @self)))

(defn Matrix-str
  [self] (try (when (py-truthy (py-eq (Matrix-num-rows self) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value "[]"}))) (when (py-truthy (py-eq (Matrix-num-rows self) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-add "[[" (clojure.string/join ". " (map py-str (py-str (py-get (:rows @self) 0))))) "]]")}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-add "[" (clojure.string/join "\n " (map py-str (vec (for [row (py-iter (:rows @self))] (py-add (py-add "[" (clojure.string/join ". " (map py-str (vec (for [value (py-iter row)] (py-str value)))))) ".]")))))) "]")})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-add-row
  ([self row] (Matrix-add-row self row nil))
  ([self row position] (let [type-error (atom nil) value (atom nil)] (reset! type-error (ex-info (py-str "Row must be a list containing all ints and/or floats") {:type :TypeError})) (when (py-truthy (not (py-truthy (sequential? row)))) (throw @type-error)) (doseq [__i1 (py-iter row)] (reset! value __i1) (when (py-truthy (not (py-truthy (or (integer? @value) (number? @value))))) (throw @type-error))) (when (py-truthy (not (py-eq (count row) (Matrix-num-columns self)))) (throw (ex-info (py-str "Row must be equal in length to the other rows in the matrix") {:type :ValueError}))) (if (py-truthy (nil? position)) (do (conj (vec (:rows @self)) row)) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] (vec (concat (seq (py-slice (:rows @self) 0 position nil)) (list row) (seq (py-slice (:rows @self) position nil nil))))))))) nil)))

(defn Matrix-add-column
  ([self column] (Matrix-add-column self column nil))
  ([self column position] (let [type-error (atom nil) value (atom nil)] (reset! type-error (ex-info (py-str "Column must be a list containing all ints and/or floats") {:type :TypeError})) (when (py-truthy (not (py-truthy (sequential? column)))) (throw @type-error)) (doseq [__i1 (py-iter column)] (reset! value __i1) (when (py-truthy (not (py-truthy (or (integer? @value) (number? @value))))) (throw @type-error))) (when (py-truthy (not (py-eq (count column) (Matrix-num-rows self)))) (throw (ex-info (py-str "Column must be equal in length to the other columns in the matrix") {:type :ValueError}))) (if (py-truthy (nil? position)) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] (vec (for [i (py-iter (range (Matrix-num-rows self)))] (py-add (py-get (:rows @self) i) [(py-get column i)]))))))) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] (vec (for [i (py-iter (range (Matrix-num-rows self)))] (vec (concat (seq (py-slice (py-get (:rows @self) i) 0 position nil)) (list (py-get column i)) (seq (py-slice (py-get (:rows @self) i) position nil nil))))))))))) nil)))

(defn Matrix-eq
  [self other] (try (when (py-truthy (not (py-truthy (instance? Matrix other)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value :py-not-implemented}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq (:rows @self) (:rows other))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-ne
  [self other] (not (py-truthy (py-eq @self other))))

(defn Matrix-neg
  [self] (py-mul @self (py-neg 1)))

(defn Matrix-add
  [self other] (try (when (py-truthy (not (py-eq (Matrix-order self) (Matrix-order (atom other))))) (throw (ex-info (py-str "Addition requires matrices of the same order") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [i (py-iter (range (Matrix-num-rows self)))] (vec (for [j (py-iter (range (Matrix-num-columns self)))] (py-add (py-get (py-get (:rows @self) i) j) (py-get (py-get (:rows other) i) j)))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-sub
  [self other] (try (when (py-truthy (not (py-eq (Matrix-order self) (Matrix-order (atom other))))) (throw (ex-info (py-str "Subtraction requires matrices of the same order") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [i (py-iter (range (Matrix-num-rows self)))] (vec (for [j (py-iter (range (Matrix-num-columns self)))] (py-sub (py-get (py-get (:rows @self) i) j) (py-get (py-get (:rows other) i) j)))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-mul
  [self other] (try (if (py-truthy (let [__b1 (integer? other)] (if (py-truthy __b1) __b1 (number? other)))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [row (py-iter (:rows @self))] (vec (for [element (py-iter row)] (py-int (py-mul element other) nil))))))}))) (do (if (py-truthy (instance? Matrix other)) (do (when (py-truthy (not (py-eq (Matrix-num-columns self) (Matrix-num-rows (atom other))))) (throw (ex-info (py-str "The number of columns in the first matrix must be equal to the number of rows in the second") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [row (py-iter (:rows @self))] (vec (for [column (py-iter (Matrix-columns (atom other)))] (Matrix-dot-product row column))))))}))) (do (throw (ex-info (py-str "A Matrix can only be multiplied by an int, float, or another matrix") {:type :TypeError})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-pow
  [self other] (let [--v (atom nil) result (atom nil)] (try (when (py-truthy (not (py-truthy (integer? other)))) (throw (ex-info (py-str "A Matrix can only be raised to the power of an int") {:type :TypeError}))) (when (py-truthy (not (py-truthy (Matrix-is-square self)))) (throw (ex-info (py-str "Only square matrices can be raised to a power") {:type :ValueError}))) (when (py-truthy (py-eq other 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Matrix-identity self)}))) (when (py-truthy (py-lt other 0)) (when (py-truthy (Matrix-is-invertable self)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-pow (Matrix-inverse self) (py-neg other))}))) (throw (ex-info (py-str "Only invertable matrices can be raised to a negative power") {:type :ValueError}))) (reset! result @self) (doseq [__i1 (py-iter (range (py-sub other 1)))] (reset! --v __i1) (swap! result py-mul @self)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-dot-product
  [row column] (reduce +' 0 (doall (for [i (py-iter (range (count row)))] (py-mul (py-get row i) (py-get column i))))))

(defmethod print-method Matrix [o w] (.write w (if *print-readably* (Matrix-repr (atom o)) (Matrix-str (atom o)))))

(defmethod py-dunder [:eq Matrix] [_ a b] (Matrix-eq (atom a) b))

(defmethod py-dunder [:neg Matrix] [_ a] (Matrix-neg (atom a)))

(defmethod py-dunder [:add Matrix] [_ a b] (Matrix-add (atom a) b))

(defmethod py-dunder [:sub Matrix] [_ a b] (Matrix-sub (atom a) b))

(defmethod py-dunder [:mul Matrix] [_ a b] (Matrix-mul (atom a) b))

(defmethod py-dunder [:pow Matrix] [_ a b] (Matrix-pow (atom a) b))
