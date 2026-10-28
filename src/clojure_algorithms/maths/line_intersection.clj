(ns clojure-algorithms.maths.line-intersection
  "Port of maths/line_intersection.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare determinant line-coefficients segment-intersection)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-sub py-repeat-vec py-mul py-div py-neg py-min py-min-by py-max py-max-by py-float-str py-quote py-repr)

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

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

(defn determinant "Calculates the determinant of a 2x2 matrix:\n\n| m00  m01 |\n| m10  m11 |\n\nArgs:\n    m00 (float): Element in the first row, first column.\n    m01 (float): Element in the first row, second column.\n    m10 (float): Element in the second row, first column.\n    m11 (float): Element in the second row, second column.\n\nReturns:\n    float: The determinant of the matrix.\n\nExamples:\n    # Determinant of the identity matrix (should be 1)\n    >>> determinant(1, 0, 0, 1)\n    1\n\n    # Determinant of a matrix with two equal rows (should be 0)\n    >>> determinant(1, 2, 1, 2)\n    0\n\n    # Determinant of a matrix with a negative determinant\n    >>> determinant(1, 2, 3, 4)\n    -2\n\n    # Determinant of a matrix with larger numbers\n    >>> determinant(10, 20, 30, 40)\n    -200"
  [m00 m01 m10 m11] (py-sub (py-mul m00 m11) (py-mul m10 m01)))

(defn line-coefficients "Computes the coefficients A, B, C of the line equation Ax + By + C = 0\nfrom two points.\n\nArgs:\n    p1 (List[float] | tuple): First point (x, y).\n    p2 (List[float] | tuple): Second point (x, y).\n\nReturns:\n    tuple: Coefficients (A, B, C) of the line equation.\n\nExamples:\n    # Vertical line (x = constant)\n    >>> line_coefficients([1, 0], [1, 2])\n    (1, 0, 1)\n\n    # Horizontal line (y = constant)\n    >>> line_coefficients([0, 1], [2, 1])\n    (0.0, -1, 1.0)\n\n    # Diagonal line (positive slope)\n    >>> line_coefficients([0, 0], [1, 1])\n    (1.0, -1, 0.0)\n\n    # Diagonal line (negative slope)\n    >>> line_coefficients([0, 1], [1, 0])\n    (-1.0, -1, 1.0)"
  [p1 p2] (let [a (atom nil) b (atom nil) c (atom nil)] (try (if (py-truthy (py-eq (py-get p1 0) (py-get p2 0))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value [1 0 (py-get p1 0)]}))) (do (reset! a (py-div (py-sub (py-get p2 1) (py-get p1 1)) (py-sub (py-get p2 0) (py-get p1 0)))) (reset! b (py-neg 1)) (reset! c (py-sub (py-get p2 1) (py-mul @a (py-get p2 0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@a @b @c]})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn segment-intersection "Finds the intersection point of two line segments or lines, if it exists.\n\nArgs:\n    v1 (List[float] | tuple): First point of the first segment (x, y).\n    v2 (List[float] | tuple): Second point of the first segment (x, y).\n    v1_prime (List[float] | tuple): First point of the second segment (x, y).\n    v2_prime (List[float] | tuple): Second point of the second segment (x, y).\n    as_segments (bool):\n        treat the inputs as line segments (True)\n        or as infinite lines (False).\n\nReturns:\n    List[float] | None:\n        Returns the intersection point [x, y] if existent, otherwise None.\n\nReferences:\n    Cramer's rule: https://en.wikipedia.org/wiki/Cramer%27s_rule\n\nExamples:\n    >>> segment_intersection([0, 0], [1, 1], [1, 0], [0, 1])\n    [0.5, 0.5]\n\n    # No intersection\n    >>> segment_intersection([0, 0], [1, 1], [2, 2], [3, 3]) is None\n    True\n\n    # Parallel lines\n    >>> segment_intersection([0, 0], [0, 1], [1, 0], [1, 1]) is None\n    True\n\n    # Intersecting infinite lines\n    >>> segment_intersection([0, 0], [1, 1], [1, 0], [0, 1], as_segments=False)\n    [0.5, 0.5]\n\n    # Parallel infinite lines (ignoring segment boundaries)\n    >>> segment_intersection([0, 0], [1, 1], [2, 2], [3, 3], False) is None\n    True"
  ([v1 v2 v1-prime v2-prime] (segment-intersection v1 v2 v1-prime v2-prime true))
  ([v1 v2 v1-prime v2-prime as-segments] (let [a (atom nil) a-prime (atom nil) b (atom nil) b-prime (atom nil) c (atom nil) c-prime (atom nil) d (atom nil) dx (atom nil) dy (atom nil) x (atom nil) y (atom nil)] (try (let [__d1 (vec (line-coefficients v1 v2))] (reset! a (nth __d1 0)) (reset! b (nth __d1 1)) (reset! c (nth __d1 2))) (let [__d2 (vec (line-coefficients v1-prime v2-prime))] (reset! a-prime (nth __d2 0)) (reset! b-prime (nth __d2 1)) (reset! c-prime (nth __d2 2))) (reset! d (determinant @a @b @a-prime @b-prime)) (when (py-truthy (py-eq @d 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (reset! dx (determinant (py-neg @c) @b (py-neg @c-prime) @b-prime)) (reset! dy (determinant @a (py-neg @c) @a-prime (py-neg @c-prime))) (let [__d3 (vec [(py-div @dx @d) (py-div @dy @d)])] (reset! x (nth __d3 0)) (reset! y (nth __d3 1))) (if (py-truthy as-segments) (do (when (py-truthy (let [__b18 (let [__c4 (py-min [(py-get v1 0) (py-get v2 0)]) __c5 @x __c6 (py-max [(py-get v1 0) (py-get v2 0)])] (and (py-le __c4 __c5) (py-le __c5 __c6)))] (if (py-truthy __b18) (let [__b17 (let [__c7 (py-min [(py-get v1-prime 0) (py-get v2-prime 0)]) __c8 @x __c9 (py-max [(py-get v1-prime 0) (py-get v2-prime 0)])] (and (py-le __c7 __c8) (py-le __c8 __c9)))] (if (py-truthy __b17) (let [__b16 (let [__c10 (py-min [(py-get v1 1) (py-get v2 1)]) __c11 @y __c12 (py-max [(py-get v1 1) (py-get v2 1)])] (and (py-le __c10 __c11) (py-le __c11 __c12)))] (if (py-truthy __b16) (let [__c13 (py-min [(py-get v1-prime 1) (py-get v2-prime 1)]) __c14 @y __c15 (py-max [(py-get v1-prime 1) (py-get v2-prime 1)])] (and (py-le __c13 __c14) (py-le __c14 __c15))) __b16)) __b17)) __b18))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@x @y]}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@x @y]})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
