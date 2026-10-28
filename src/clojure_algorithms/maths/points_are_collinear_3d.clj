(ns clojure-algorithms.maths.points-are-collinear-3d
  "Check if three points are collinear in 3D.\n\nIn short, the idea is that we are able to create a triangle using three points,\nand the area of that triangle can determine if the three points are collinear or not.\n\n\nFirst, we create two vectors with the same initial point from the three points,\nthen we will calculate the cross-product of them.\n\nThe length of the cross vector is numerically equal to the area of a parallelogram.\n\nFinally, the area of the triangle is equal to half of the area of the parallelogram.\n\nSince we are only differentiating between zero and anything else,\nwe can get rid of the square root when calculating the length of the vector,\nand also the division by two at the end.\n\nFrom a second perspective, if the two vectors are parallel and overlapping,\nwe can't get a nonzero perpendicular vector,\nsince there will be an infinite number of orthogonal vectors.\n\nTo simplify the solution we will not calculate the length,\nbut we will decide directly from the vector whether it is equal to (0, 0, 0) or not.\n\n\nRead More:\n    https://math.stackexchange.com/a/1951650"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare create-vector get-3d-vectors-cross is-zero-vector are-collinear Vector3d Point3d)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-sub py-repeat-vec py-mul py-neg py-list py-float-str py-quote py-repr py-round)

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

(defn create-vector "Pass two points to get the vector from them in the form (x, y, z).\n\n>>> create_vector((0, 0, 0), (1, 1, 1))\n(1, 1, 1)\n>>> create_vector((45, 70, 24), (47, 32, 1))\n(2, -38, -23)\n>>> create_vector((-14, -1, -8), (-7, 6, 4))\n(7, 7, 12)"
  [end-point1 end-point2] (let [x (atom nil) y (atom nil) z (atom nil)] (try (reset! x (py-sub (py-get end-point2 0) (py-get end-point1 0))) (reset! y (py-sub (py-get end-point2 1) (py-get end-point1 1))) (reset! z (py-sub (py-get end-point2 2) (py-get end-point1 2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@x @y @z]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn get-3d-vectors-cross "Get the cross of the two vectors AB and AC.\n\nI used determinant of 2x2 to get the determinant of the 3x3 matrix in the process.\n\nRead More:\n    https://en.wikipedia.org/wiki/Cross_product\n    https://en.wikipedia.org/wiki/Determinant\n\n>>> get_3d_vectors_cross((3, 4, 7), (4, 9, 2))\n(-55, 22, 11)\n>>> get_3d_vectors_cross((1, 1, 1), (1, 1, 1))\n(0, 0, 0)\n>>> get_3d_vectors_cross((-4, 3, 0), (3, -9, -12))\n(-36, -48, 27)\n>>> get_3d_vectors_cross((17.67, 4.7, 6.78), (-9.5, 4.78, -19.33))\n(-123.2594, 277.15110000000004, 129.11260000000001)"
  [ab ac] (let [x (atom nil) y (atom nil) z (atom nil)] (try (reset! x (py-sub (py-mul (py-get ab 1) (py-get ac 2)) (py-mul (py-get ab 2) (py-get ac 1)))) (reset! y (py-mul (py-sub (py-mul (py-get ab 0) (py-get ac 2)) (py-mul (py-get ab 2) (py-get ac 0))) (py-neg 1))) (reset! z (py-sub (py-mul (py-get ab 0) (py-get ac 1)) (py-mul (py-get ab 1) (py-get ac 0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@x @y @z]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn is-zero-vector "Check if vector is equal to (0, 0, 0) or not.\n\nSince the algorithm is very accurate, we will never get a zero vector,\nso we need to round the vector axis,\nbecause we want a result that is either True or False.\nIn other applications, we can return a float that represents the collinearity ratio.\n\n>>> is_zero_vector((0, 0, 0), accuracy=10)\nTrue\n>>> is_zero_vector((15, 74, 32), accuracy=10)\nFalse\n>>> is_zero_vector((-15, -74, -32), accuracy=10)\nFalse"
  [vector-v accuracy] (py-eq (py-list (doall (for [x (py-iter vector-v)] (py-round x accuracy)))) [0 0 0]))

(defn are-collinear "Check if three points are collinear or not.\n\n1- Create two vectors AB and AC.\n2- Get the cross vector of the two vectors.\n3- Calculate the length of the cross vector.\n4- If the length is zero then the points are collinear, else they are not.\n\nThe use of the accuracy parameter is explained in is_zero_vector docstring.\n\n>>> are_collinear((4.802293498137402, 3.536233125455244, 0),\n...               (-2.186788107953106, -9.24561398001649, 7.141509524846482),\n...               (1.530169574640268, -2.447927606600034, 3.343487096469054))\nTrue\n>>> are_collinear((-6, -2, 6),\n...               (6.200213806439997, -4.930157614926678, -4.482371908289856),\n...               (-4.085171149525941, -2.459889509029438, 4.354787180795383))\nTrue\n>>> are_collinear((2.399001826862445, -2.452009976680793, 4.464656666157666),\n...               (-3.682816335934376, 5.753788986533145, 9.490993909044244),\n...               (1.962903518985307, 3.741415730125627, 7))\nFalse\n>>> are_collinear((1.875375340689544, -7.268426006071538, 7.358196269835993),\n...               (-3.546599383667157, -4.630005261513976, 3.208784032924246),\n...               (-2.564606140206386, 3.937845170672183, 7))\nFalse"
  ([a b c] (are-collinear a b c 10))
  ([a b c accuracy] (let [ab (atom nil) ac (atom nil)] (try (reset! ab (create-vector a b)) (reset! ac (create-vector a c)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (is-zero-vector (get-3d-vectors-cross @ab @ac) accuracy)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(def Vector3d nil)

(def Point3d nil)

;; NOTE: const Vector3d is a typing alias
;; NOTE: const Point3d is a typing alias
