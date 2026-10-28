(ns clojure-algorithms.maths.volume
  "Find the volume of various shapes.\n\n* https://en.wikipedia.org/wiki/Volume\n* https://en.wikipedia.org/wiki/Spherical_cap"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare vol-cube vol-spherical-cap vol-spheres-intersect vol-spheres-union vol-cuboid vol-cone vol-right-circ-cone vol-prism vol-pyramid vol-sphere vol-hemisphere vol-circular-cylinder vol-hollow-circular-cylinder vol-conical-frustum vol-torus vol-octahedron vol-icosahedron main)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-repeat-vec py-mul py-div py-pow py-min py-min-by py-max py-max-by py-float-str py-quote py-repr py-str py-float)

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

(defn vol-cube "Calculate the Volume of a Cube.\n\n>>> vol_cube(1)\n1.0\n>>> vol_cube(3)\n27.0\n>>> vol_cube(0)\n0.0\n>>> vol_cube(1.6)\n4.096000000000001\n>>> vol_cube(-1)\nTraceback (most recent call last):\n    ...\nValueError: vol_cube() only accepts non-negative values"
  [side-length] (try (when (py-truthy (py-lt side-length 0)) (throw (ex-info (py-str "vol_cube() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-pow side-length 3)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-spherical-cap "Calculate the volume of the spherical cap.\n\n>>> vol_spherical_cap(1, 2)\n5.235987755982988\n>>> vol_spherical_cap(1.6, 2.6)\n16.621119532592402\n>>> vol_spherical_cap(0, 0)\n0.0\n>>> vol_spherical_cap(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: vol_spherical_cap() only accepts non-negative values\n>>> vol_spherical_cap(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: vol_spherical_cap() only accepts non-negative values"
  [height radius] (try (when (py-truthy (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt radius 0)))) (throw (ex-info (py-str "vol_spherical_cap() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-mul (py-div 1 3) Math/PI) (py-pow height 2)) (py-sub (py-mul 3 radius) height))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-spheres-intersect "Calculate the volume of the intersection of two spheres.\n\nThe intersection is composed by two spherical caps and therefore its volume is the\nsum of the volumes of the spherical caps.\nFirst, it calculates the heights :math:`(h_1, h_2)` of the spherical caps,\nthen the two volumes and it returns the sum.\nThe height formulas are\n\n.. math::\n    h_1 = \\frac{(radius_1 - radius_2 + centers\\_distance)\n                \\cdot (radius_1 + radius_2 - centers\\_distance)}\n               {2 \\cdot centers\\_distance}\n\n    h_2 = \\frac{(radius_2 - radius_1 + centers\\_distance)\n                \\cdot (radius_2 + radius_1 - centers\\_distance)}\n               {2 \\cdot centers\\_distance}\n\nif `centers_distance` is 0 then it returns the volume of the smallers sphere\n\n:return: ``vol_spherical_cap`` (:math:`h_1`, :math:`radius_2`)\n         + ``vol_spherical_cap`` (:math:`h_2`, :math:`radius_1`)\n\n>>> vol_spheres_intersect(2, 2, 1)\n21.205750411731103\n>>> vol_spheres_intersect(2.6, 2.6, 1.6)\n40.71504079052372\n>>> vol_spheres_intersect(0, 0, 0)\n0.0\n>>> vol_spheres_intersect(-2, 2, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_spheres_intersect() only accepts non-negative values\n>>> vol_spheres_intersect(2, -2, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_spheres_intersect() only accepts non-negative values\n>>> vol_spheres_intersect(2, 2, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_spheres_intersect() only accepts non-negative values"
  [radius-1 radius-2 centers-distance] (let [h1 (atom nil) h2 (atom nil)] (try (when (py-truthy (let [__b2 (py-lt radius-1 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt radius-2 0)] (if (py-truthy __b1) __b1 (py-lt centers-distance 0)))))) (throw (ex-info (py-str "vol_spheres_intersect() only accepts non-negative values") {:type :ValueError}))) (when (py-truthy (py-eq centers-distance 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vol-sphere (py-min [radius-1 radius-2]))}))) (reset! h1 (py-div (py-mul (py-add (py-sub radius-1 radius-2) centers-distance) (py-sub (py-add radius-1 radius-2) centers-distance)) (py-mul 2 centers-distance))) (reset! h2 (py-div (py-mul (py-add (py-sub radius-2 radius-1) centers-distance) (py-sub (py-add radius-2 radius-1) centers-distance)) (py-mul 2 centers-distance))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (vol-spherical-cap @h1 radius-2) (vol-spherical-cap @h2 radius-1))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn vol-spheres-union "Calculate the volume of the union of two spheres that possibly intersect.\n\nIt is the sum of sphere :math:`A` and sphere :math:`B` minus their intersection.\nFirst, it calculates the volumes :math:`(v_1, v_2)` of the spheres,\nthen the volume of the intersection :math:`i` and\nit returns the sum :math:`v_1 + v_2 - i`.\nIf `centers_distance` is 0 then it returns the volume of the larger sphere\n\n:return: ``vol_sphere`` (:math:`radius_1`) + ``vol_sphere`` (:math:`radius_2`)\n         - ``vol_spheres_intersect``\n         (:math:`radius_1`, :math:`radius_2`, :math:`centers\\_distance`)\n\n>>> vol_spheres_union(2, 2, 1)\n45.814892864851146\n>>> vol_spheres_union(1.56, 2.2, 1.4)\n48.77802773671288\n>>> vol_spheres_union(0, 2, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_spheres_union() only accepts non-negative values, non-zero radius\n>>> vol_spheres_union('1.56', '2.2', '1.4')\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'str' and 'int'\n>>> vol_spheres_union(1, None, 1)\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'NoneType' and 'int'"
  [radius-1 radius-2 centers-distance] (try (when (py-truthy (let [__b2 (py-le radius-1 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-le radius-2 0)] (if (py-truthy __b1) __b1 (py-lt centers-distance 0)))))) (throw (ex-info (py-str "vol_spheres_union() only accepts non-negative values, non-zero radius") {:type :ValueError}))) (when (py-truthy (py-eq centers-distance 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vol-sphere (py-max [radius-1 radius-2]))}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sub (py-add (vol-sphere radius-1) (vol-sphere radius-2)) (vol-spheres-intersect radius-1 radius-2 centers-distance))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-cuboid "Calculate the Volume of a Cuboid.\n\n:return: multiple of `width`, `length` and `height`\n\n>>> vol_cuboid(1, 1, 1)\n1.0\n>>> vol_cuboid(1, 2, 3)\n6.0\n>>> vol_cuboid(1.6, 2.6, 3.6)\n14.976\n>>> vol_cuboid(0, 0, 0)\n0.0\n>>> vol_cuboid(-1, 2, 3)\nTraceback (most recent call last):\n    ...\nValueError: vol_cuboid() only accepts non-negative values\n>>> vol_cuboid(1, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: vol_cuboid() only accepts non-negative values\n>>> vol_cuboid(1, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: vol_cuboid() only accepts non-negative values"
  [width height length] (try (when (py-truthy (let [__b2 (py-lt width 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt length 0)))))) (throw (ex-info (py-str "vol_cuboid() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-float (py-mul (py-mul width height) length))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-cone "| Calculate the Volume of a Cone.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Cone\n\n:return: :math:`\\frac{1}{3} \\cdot area\\_of\\_base \\cdot height`\n\n>>> vol_cone(10, 3)\n10.0\n>>> vol_cone(1, 1)\n0.3333333333333333\n>>> vol_cone(1.6, 1.6)\n0.8533333333333335\n>>> vol_cone(0, 0)\n0.0\n>>> vol_cone(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_cone() only accepts non-negative values\n>>> vol_cone(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_cone() only accepts non-negative values"
  [area-of-base height] (try (when (py-truthy (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt area-of-base 0)))) (throw (ex-info (py-str "vol_cone() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul area-of-base height) 3.0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-right-circ-cone "| Calculate the Volume of a Right Circular Cone.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Cone\n\n:return: :math:`\\frac{1}{3} \\cdot \\pi \\cdot radius^2 \\cdot height`\n\n>>> vol_right_circ_cone(2, 3)\n12.566370614359172\n>>> vol_right_circ_cone(0, 0)\n0.0\n>>> vol_right_circ_cone(1.6, 1.6)\n4.289321169701265\n>>> vol_right_circ_cone(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_right_circ_cone() only accepts non-negative values\n>>> vol_right_circ_cone(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_right_circ_cone() only accepts non-negative values"
  [radius height] (try (when (py-truthy (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt radius 0)))) (throw (ex-info (py-str "vol_right_circ_cone() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul (py-mul Math/PI (py-pow radius 2)) height) 3.0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-prism "| Calculate the Volume of a Prism.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Prism_(geometry)\n\n:return: :math:`V = B \\cdot h`\n\n>>> vol_prism(10, 2)\n20.0\n>>> vol_prism(11, 1)\n11.0\n>>> vol_prism(1.6, 1.6)\n2.5600000000000005\n>>> vol_prism(0, 0)\n0.0\n>>> vol_prism(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_prism() only accepts non-negative values\n>>> vol_prism(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_prism() only accepts non-negative values"
  [area-of-base height] (try (when (py-truthy (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt area-of-base 0)))) (throw (ex-info (py-str "vol_prism() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-float (py-mul area-of-base height))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-pyramid "| Calculate the Volume of a Pyramid.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Pyramid_(geometry)\n\n:return: :math:`\\frac{1}{3} \\cdot B \\cdot h`\n\n>>> vol_pyramid(10, 3)\n10.0\n>>> vol_pyramid(1.5, 3)\n1.5\n>>> vol_pyramid(1.6, 1.6)\n0.8533333333333335\n>>> vol_pyramid(0, 0)\n0.0\n>>> vol_pyramid(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_pyramid() only accepts non-negative values\n>>> vol_pyramid(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_pyramid() only accepts non-negative values"
  [area-of-base height] (try (when (py-truthy (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt area-of-base 0)))) (throw (ex-info (py-str "vol_pyramid() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul area-of-base height) 3.0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-sphere "| Calculate the Volume of a Sphere.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Sphere\n\n:return: :math:`\\frac{4}{3} \\cdot \\pi \\cdot r^3`\n\n>>> vol_sphere(5)\n523.5987755982989\n>>> vol_sphere(1)\n4.1887902047863905\n>>> vol_sphere(1.6)\n17.15728467880506\n>>> vol_sphere(0)\n0.0\n>>> vol_sphere(-1)\nTraceback (most recent call last):\n    ...\nValueError: vol_sphere() only accepts non-negative values"
  [radius] (try (when (py-truthy (py-lt radius 0)) (throw (ex-info (py-str "vol_sphere() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-div 4 3) Math/PI) (py-pow radius 3))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-hemisphere "| Calculate the volume of a hemisphere\n| Wikipedia reference: https://en.wikipedia.org/wiki/Hemisphere\n| Other references: https://www.cuemath.com/geometry/hemisphere\n\n:return: :math:`\\frac{2}{3} \\cdot \\pi \\cdot radius^3`\n\n>>> vol_hemisphere(1)\n2.0943951023931953\n>>> vol_hemisphere(7)\n718.377520120866\n>>> vol_hemisphere(1.6)\n8.57864233940253\n>>> vol_hemisphere(0)\n0.0\n>>> vol_hemisphere(-1)\nTraceback (most recent call last):\n    ...\nValueError: vol_hemisphere() only accepts non-negative values"
  [radius] (try (when (py-truthy (py-lt radius 0)) (throw (ex-info (py-str "vol_hemisphere() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul (py-mul (py-pow radius 3) Math/PI) 2) 3)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-circular-cylinder "| Calculate the Volume of a Circular Cylinder.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Cylinder\n\n:return: :math:`\\pi \\cdot radius^2 \\cdot height`\n\n>>> vol_circular_cylinder(1, 1)\n3.141592653589793\n>>> vol_circular_cylinder(4, 3)\n150.79644737231007\n>>> vol_circular_cylinder(1.6, 1.6)\n12.867963509103795\n>>> vol_circular_cylinder(0, 0)\n0.0\n>>> vol_circular_cylinder(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_circular_cylinder() only accepts non-negative values\n>>> vol_circular_cylinder(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_circular_cylinder() only accepts non-negative values"
  [radius height] (try (when (py-truthy (let [__b1 (py-lt height 0)] (if (py-truthy __b1) __b1 (py-lt radius 0)))) (throw (ex-info (py-str "vol_circular_cylinder() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-pow radius 2) height) Math/PI)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-hollow-circular-cylinder "Calculate the Volume of a Hollow Circular Cylinder.\n\n>>> vol_hollow_circular_cylinder(1, 2, 3)\n28.274333882308138\n>>> vol_hollow_circular_cylinder(1.6, 2.6, 3.6)\n47.50088092227767\n>>> vol_hollow_circular_cylinder(-1, 2, 3)\nTraceback (most recent call last):\n    ...\nValueError: vol_hollow_circular_cylinder() only accepts non-negative values\n>>> vol_hollow_circular_cylinder(1, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: vol_hollow_circular_cylinder() only accepts non-negative values\n>>> vol_hollow_circular_cylinder(1, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: vol_hollow_circular_cylinder() only accepts non-negative values\n>>> vol_hollow_circular_cylinder(2, 1, 3)\nTraceback (most recent call last):\n    ...\nValueError: outer_radius must be greater than inner_radius\n>>> vol_hollow_circular_cylinder(0, 0, 0)\nTraceback (most recent call last):\n    ...\nValueError: outer_radius must be greater than inner_radius"
  [inner-radius outer-radius height] (try (when (py-truthy (let [__b2 (py-lt inner-radius 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt outer-radius 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))))) (throw (ex-info (py-str "vol_hollow_circular_cylinder() only accepts non-negative values") {:type :ValueError}))) (when (py-truthy (py-le outer-radius inner-radius)) (throw (ex-info (py-str "outer_radius must be greater than inner_radius") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul Math/PI (py-sub (py-pow outer-radius 2) (py-pow inner-radius 2))) height)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-conical-frustum "| Calculate the Volume of a Conical Frustum.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Frustum\n\n>>> vol_conical_frustum(45, 7, 28)\n48490.482608158454\n>>> vol_conical_frustum(1, 1, 2)\n7.330382858376184\n>>> vol_conical_frustum(1.6, 2.6, 3.6)\n48.7240076620753\n>>> vol_conical_frustum(0, 0, 0)\n0.0\n>>> vol_conical_frustum(-2, 2, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_conical_frustum() only accepts non-negative values\n>>> vol_conical_frustum(2, -2, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_conical_frustum() only accepts non-negative values\n>>> vol_conical_frustum(2, 2, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_conical_frustum() only accepts non-negative values"
  [height radius-1 radius-2] (try (when (py-truthy (let [__b2 (py-lt radius-1 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt radius-2 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))))) (throw (ex-info (py-str "vol_conical_frustum() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-mul (py-div 1 3) Math/PI) height) (py-add (py-add (py-pow radius-1 2) (py-pow radius-2 2)) (py-mul radius-1 radius-2)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-torus "| Calculate the Volume of a Torus.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Torus\n\n:return: :math:`2 \\pi^2 \\cdot torus\\_radius \\cdot tube\\_radius^2`\n\n>>> vol_torus(1, 1)\n19.739208802178716\n>>> vol_torus(4, 3)\n710.6115168784338\n>>> vol_torus(3, 4)\n947.4820225045784\n>>> vol_torus(1.6, 1.6)\n80.85179925372404\n>>> vol_torus(0, 0)\n0.0\n>>> vol_torus(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: vol_torus() only accepts non-negative values\n>>> vol_torus(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: vol_torus() only accepts non-negative values"
  [torus-radius tube-radius] (try (when (py-truthy (let [__b1 (py-lt torus-radius 0)] (if (py-truthy __b1) __b1 (py-lt tube-radius 0)))) (throw (ex-info (py-str "vol_torus() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-mul 2 (py-pow Math/PI 2)) torus-radius) (py-pow tube-radius 2))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-octahedron "| Calculate the Volume of an Octahedron.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Regular_octahedron\n\n>>> from math import isclose\n>>> isclose(vol_octahedron(2.5), 7.36569563735987)\nTrue\n>>> isclose(vol_octahedron(10), 471.40452079103168)\nTrue\n>>> isclose(vol_octahedron(5), 58.92556509887896)\nTrue\n>>> isclose(vol_octahedron(3.5), 20.21146882891548)\nTrue\n>>> vol_octahedron(0)\n0.0\n>>> vol_octahedron(-1)\nTraceback (most recent call last):\n    ...\nValueError: vol_octahedron() only accepts non-negative values"
  [oct-side] (try (when (py-truthy (py-lt oct-side 0)) (throw (ex-info (py-str "vol_octahedron() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul (py-pow oct-side 3) (py-pow 2 0.5)) 3)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn vol-icosahedron "| Calculate the Volume of an Icosahedron.\n| Wikipedia reference: https://en.wikipedia.org/wiki/Regular_icosahedron\n\n>>> from math import isclose\n>>> isclose(vol_icosahedron(2.5), 34.088984228514256)\nTrue\n>>> isclose(vol_icosahedron(10), 2181.694990624912374)\nTrue\n>>> isclose(vol_icosahedron(5), 272.711873828114047)\nTrue\n>>> isclose(vol_icosahedron(3.49), 92.740688412033628)\nTrue\n>>> vol_icosahedron(0)\n0.0\n>>> vol_icosahedron(-1)\nTraceback (most recent call last):\n    ...\nValueError: vol_icosahedron() only accepts non-negative values\n>>> vol_icosahedron(-0.2)\nTraceback (most recent call last):\n    ...\nValueError: vol_icosahedron() only accepts non-negative values"
  [tri-side] (try (when (py-truthy (py-lt tri-side 0)) (throw (ex-info (py-str "vol_icosahedron() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul (py-mul (py-pow tri-side 3) (py-add 3 (py-pow 5 0.5))) 5) 12)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn main "Print the Results of Various Volume Calculations."
  [] (println (clojure.string/join " " [(py-str "Volumes:")])) (println (clojure.string/join " " [(py-str (str "Cube: vol_cube(2) = " (py-str (pr-str (vol-cube 2)))))])) (println (clojure.string/join " " [(py-str (str "Cuboid: vol_cuboid(2, 2, 2) = " (py-str (pr-str (vol-cuboid 2 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Cone: vol_cone(2, 2) = " (py-str (pr-str (vol-cone 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Right Circular Cone: vol_right_circ_cone(2, 2) = " (py-str (pr-str (vol-right-circ-cone 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Prism: vol_prism(2, 2) = " (py-str (pr-str (vol-prism 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Pyramid: vol_pyramid(2, 2) = " (py-str (pr-str (vol-pyramid 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Sphere: vol_sphere(2) = " (py-str (pr-str (vol-sphere 2)))))])) (println (clojure.string/join " " [(py-str (str "Hemisphere: vol_hemisphere(2) = " (py-str (pr-str (vol-hemisphere 2)))))])) (println (clojure.string/join " " [(py-str (str "Circular Cylinder: vol_circular_cylinder(2, 2) = " (py-str (pr-str (vol-circular-cylinder 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Torus: vol_torus(2, 2) = " (py-str (pr-str (vol-torus 2 2)))))])) (println (clojure.string/join " " [(py-str (str "Conical Frustum: vol_conical_frustum(2, 2, 4) = " (py-str (pr-str (vol-conical-frustum 2 2 4)))))])) (println (clojure.string/join " " [(py-str (str "Spherical cap: vol_spherical_cap(1, 2) = " (py-str (pr-str (vol-spherical-cap 1 2)))))])) (println (clojure.string/join " " [(py-str (str "Spheres intersection: vol_spheres_intersect(2, 2, 1) = " (py-str (pr-str (vol-spheres-intersect 2 2 1)))))])) (println (clojure.string/join " " [(py-str (str "Spheres union: vol_spheres_union(2, 2, 1) = " (py-str (pr-str (vol-spheres-union 2 2 1)))))])) (println (clojure.string/join " " [(py-str (str "Hollow Circular Cylinder: vol_hollow_circular_cylinder(1, 2, 3) = " (py-str (pr-str (vol-hollow-circular-cylinder 1 2 3)))))])) (println (clojure.string/join " " [(py-str (str "Octahedron: vol_octahedron(2.5) = " (py-str (pr-str (vol-octahedron 2.5)))))])) (println (clojure.string/join " " [(py-str (str "Icosahedron: vol_icosahedron(2.5) = " (py-str (pr-str (vol-icosahedron 2.5)))))])) nil)
