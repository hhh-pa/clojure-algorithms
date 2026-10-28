(ns clojure-algorithms.maths.manhattan-distance
  "Port of maths/manhattan_distance.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare manhattan-distance -validate-point manhattan-distance-one-liner)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-sub py-abs py-float-str py-quote py-repr py-str py-float py-type)

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

(defn- py-abs [x]
  (cond (boolean? x) (if x 1 0)
        (number? x) (if (neg? x) (-' x) x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :abs x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for abs()" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for abs()" {:type :TypeError}))))

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

(defn- py-float [x]
  (cond (boolean? x) (if x 1.0 0.0)
        (integer? x) (double x)
        (float? x) (double x)
        (string? x)
        (let [s (-> x clojure.string/trim (clojure.string/replace "_" ""))
              l (clojure.string/lower-case s)]
          (cond (contains? #{"inf" "+inf" "infinity" "+infinity"} l) Double/POSITIVE_INFINITY
                (contains? #{"-inf" "-infinity"} l) Double/NEGATIVE_INFINITY
                (contains? #{"nan" "+nan" "-nan"} l) Double/NaN
                :else (try (Double/parseDouble s)
                           (catch NumberFormatException e
                             (throw (ex-info (str "could not convert string to float: " x) {:type :ValueError}))))))
        :else (throw (ex-info "float() argument must be a string or a number" {:type :TypeError}))))

(defn- py-type [x]
  (cond (nil? x) :none-type
        (boolean? x) :bool
        (integer? x) :int
        (float? x) :float
        (string? x) :str
        (set? x) :set
        (map? x) :dict
        (sequential? x) :list
        :else (class x)))

(defn manhattan-distance "Expectts two list of numbers representing two points in the same\nn-dimensional space\n\nhttps://en.wikipedia.org/wiki/Taxicab_geometry\n\n>>> manhattan_distance([1,1], [2,2])\n2.0\n>>> manhattan_distance([1.5,1.5], [2,2])\n1.0\n>>> manhattan_distance([1.5,1.5], [2.5,2])\n1.5\n>>> manhattan_distance([-3, -3, -3], [0, 0, 0])\n9.0\n>>> manhattan_distance([1,1], None)\nTraceback (most recent call last):\n    ...\nValueError: Missing an input\n>>> manhattan_distance([1,1], [2, 2, 2])\nTraceback (most recent call last):\n    ...\nValueError: Both points must be in the same n-dimensional space\n>>> manhattan_distance([1,\"one\"], [2, 2, 2])\nTraceback (most recent call last):\n    ...\nTypeError: Expected a list of numbers as input, found str\n>>> manhattan_distance(1, [2, 2, 2])\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found int\n>>> manhattan_distance([1,1], \"not_a_list\")\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found str"
  [point-a point-b] (try (-validate-point point-a) (-validate-point point-b) (when (py-truthy (not (py-eq (count point-a) (count point-b)))) (throw (ex-info (py-str "Both points must be in the same n-dimensional space") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-float (reduce +' 0 (doall (for [[a b] (py-iter (map vector point-a point-b))] (py-abs (py-sub a b))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn -validate-point ">>> _validate_point(None)\nTraceback (most recent call last):\n     ...\nValueError: Missing an input\n>>> _validate_point([1,\"one\"])\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found str\n>>> _validate_point(1)\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found int\n>>> _validate_point(\"not_a_list\")\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found str"
  [point] (let [item (atom nil) msg (atom nil)] (if (py-truthy point) (do (if (py-truthy (sequential? point)) (do (doseq [__i1 (py-iter point)] (reset! item __i1) (when (py-truthy (not (py-truthy (or (integer? @item) (number? @item))))) (reset! msg (str "Expected a list of numbers as input, found " (py-str (:--name-- (py-type @item))))) (throw (ex-info (py-str @msg) {:type :TypeError}))))) (do (reset! msg (str "Expected a list of numbers as input, found " (py-str (:--name-- (py-type point))))) (throw (ex-info (py-str @msg) {:type :TypeError}))))) (do (throw (ex-info (py-str "Missing an input") {:type :ValueError})))) nil))

(defn manhattan-distance-one-liner "Version with one liner\n\n>>> manhattan_distance_one_liner([1,1], [2,2])\n2.0\n>>> manhattan_distance_one_liner([1.5,1.5], [2,2])\n1.0\n>>> manhattan_distance_one_liner([1.5,1.5], [2.5,2])\n1.5\n>>> manhattan_distance_one_liner([-3, -3, -3], [0, 0, 0])\n9.0\n>>> manhattan_distance_one_liner([1,1], None)\nTraceback (most recent call last):\n     ...\nValueError: Missing an input\n>>> manhattan_distance_one_liner([1,1], [2, 2, 2])\nTraceback (most recent call last):\n     ...\nValueError: Both points must be in the same n-dimensional space\n>>> manhattan_distance_one_liner([1,\"one\"], [2, 2, 2])\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found str\n>>> manhattan_distance_one_liner(1, [2, 2, 2])\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found int\n>>> manhattan_distance_one_liner([1,1], \"not_a_list\")\nTraceback (most recent call last):\n     ...\nTypeError: Expected a list of numbers as input, found str"
  [point-a point-b] (try (-validate-point point-a) (-validate-point point-b) (when (py-truthy (not (py-eq (count point-a) (count point-b)))) (throw (ex-info (py-str "Both points must be in the same n-dimensional space") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-float (reduce +' 0 (doall (for [[x y] (py-iter (map vector point-a point-b))] (py-abs (py-sub x y))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
