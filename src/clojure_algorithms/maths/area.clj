(ns clojure-algorithms.maths.area
  "Find the area of various geometric shapes\nWikipedia reference: https://en.wikipedia.org/wiki/Area"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare surface-area-cube surface-area-cuboid surface-area-sphere surface-area-hemisphere surface-area-cone lateral-surface-cone surface-area-conical-frustum surface-area-cylinder lateral-surface-cylinder surface-area-torus area-rectangle area-square area-triangle area-triangle-three-sides area-parallelogram area-trapezium area-circle area-ellipse area-rhombus area-reg-polygon surface-area-reg-prism)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-add py-sub py-repeat-vec py-mul py-div py-pow py-powmod py-float-str py-quote py-repr py-str)

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

(defn- py-powmod [a b m]
  (let [a (py-num-bool a) b (py-num-bool b) m (py-num-bool m)]
    (when (or (not (integer? a)) (not (integer? b)) (not (integer? m)))
      (throw (ex-info "pow() 3rd argument not allowed unless all arguments are integers" {:type :TypeError})))
    (when (zero? m) (throw (ex-info "pow() 3rd argument cannot be 0" {:type :ValueError})))
    (let [bi (biginteger a) e (long b) mm (biginteger m)]
      (bigint (if (neg? b)
                (.modPow (.modInverse bi mm) (java.math.BigInteger/valueOf (- e)) mm)
                (.modPow bi (java.math.BigInteger/valueOf e) mm))))))

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

(defn surface-area-cube "Calculate the Surface Area of a Cube.\n\n>>> surface_area_cube(1)\n6\n>>> surface_area_cube(1.6)\n15.360000000000003\n>>> surface_area_cube(0)\n0\n>>> surface_area_cube(3)\n54\n>>> surface_area_cube(-1)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cube() only accepts non-negative values"
  [side-length] (try (when (py-truthy (py-lt side-length 0)) (throw (ex-info (py-str "surface_area_cube() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul 6 (py-pow side-length 2))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-cuboid "Calculate the Surface Area of a Cuboid.\n\n>>> surface_area_cuboid(1, 2, 3)\n22\n>>> surface_area_cuboid(0, 0, 0)\n0\n>>> surface_area_cuboid(1.6, 2.6, 3.6)\n38.56\n>>> surface_area_cuboid(-1, 2, 3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cuboid() only accepts non-negative values\n>>> surface_area_cuboid(1, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cuboid() only accepts non-negative values\n>>> surface_area_cuboid(1, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cuboid() only accepts non-negative values"
  [length breadth height] (try (when (py-truthy (let [__b2 (py-lt length 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt breadth 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))))) (throw (ex-info (py-str "surface_area_cuboid() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul 2 (py-add (py-add (py-mul length breadth) (py-mul breadth height)) (py-mul length height)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-sphere "Calculate the Surface Area of a Sphere.\nWikipedia reference: https://en.wikipedia.org/wiki/Sphere\nFormula: 4 * pi * r^2\n\n>>> surface_area_sphere(5)\n314.1592653589793\n>>> surface_area_sphere(1)\n12.566370614359172\n>>> surface_area_sphere(1.6)\n32.169908772759484\n>>> surface_area_sphere(0)\n0.0\n>>> surface_area_sphere(-1)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_sphere() only accepts non-negative values"
  [radius] (try (when (py-truthy (py-lt radius 0)) (throw (ex-info (py-str "surface_area_sphere() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul 4 Math/PI) (py-pow radius 2))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-hemisphere "Calculate the Surface Area of a Hemisphere.\nFormula: 3 * pi * r^2\n\n>>> surface_area_hemisphere(5)\n235.61944901923448\n>>> surface_area_hemisphere(1)\n9.42477796076938\n>>> surface_area_hemisphere(0)\n0.0\n>>> surface_area_hemisphere(1.1)\n11.40398133253095\n>>> surface_area_hemisphere(-1)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_hemisphere() only accepts non-negative values"
  [radius] (try (when (py-truthy (py-lt radius 0)) (throw (ex-info (py-str "surface_area_hemisphere() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul 3 Math/PI) (py-pow radius 2))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-cone "Calculate the Surface Area of a Cone.\nWikipedia reference: https://en.wikipedia.org/wiki/Cone\nFormula: pi * r * (r + (h ** 2 + r ** 2) ** 0.5)\n\n>>> surface_area_cone(10, 24)\n1130.9733552923256\n>>> surface_area_cone(6, 8)\n301.59289474462014\n>>> surface_area_cone(1.6, 2.6)\n23.387862992395807\n>>> surface_area_cone(0, 0)\n0.0\n>>> surface_area_cone(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cone() only accepts non-negative values\n>>> surface_area_cone(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cone() only accepts non-negative values\n>>> surface_area_cone(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cone() only accepts non-negative values"
  [radius height] (try (when (py-truthy (let [__b1 (py-lt radius 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))) (throw (ex-info (py-str "surface_area_cone() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul Math/PI radius) (py-add radius (py-pow (py-add (py-pow height 2) (py-pow radius 2)) 0.5)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn lateral-surface-cone "Calculate the Lateral Surface Area of a Cone.\nWikipedia reference: https://en.wikipedia.org/wiki/Cone\nFormula: pi * r * (h ** 2 + r ** 2) ** 0.5\n\n>>> lateral_surface_cone(10, 20)\n702.4814731040726\n>>> lateral_surface_cone(6, 8)\n188.49555921538757\n>>> lateral_surface_cone(0, 0)\n0.0\n>>> lateral_surface_cone(1, -2)\nTraceback (most recent call last):\n      ...\nValueError: lateral_surface_cone() only accepts non-negative values\n>>> lateral_surface_cone(-1, 2)\nTraceback (most recent call last):\n      ...\nValueError: lateral_surface_cone() only accepts non-negative values"
  [radius height] (try (when (py-truthy (let [__b1 (py-lt radius 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))) (throw (ex-info (py-str "lateral_surface_cone() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul Math/PI radius) (py-pow (py-add (py-pow height 2) (py-pow radius 2)) 0.5))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-conical-frustum "Calculate the Surface Area of a Conical Frustum.\n\n>>> surface_area_conical_frustum(1, 2, 3)\n45.511728065337266\n>>> surface_area_conical_frustum(4, 5, 6)\n300.7913575056268\n>>> surface_area_conical_frustum(0, 0, 0)\n0.0\n>>> surface_area_conical_frustum(1.6, 2.6, 3.6)\n78.57907060751548\n>>> surface_area_conical_frustum(-1, 2, 3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_conical_frustum() only accepts non-negative values\n>>> surface_area_conical_frustum(1, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_conical_frustum() only accepts non-negative values\n>>> surface_area_conical_frustum(1, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_conical_frustum() only accepts non-negative values"
  [radius-1 radius-2 height] (let [slant-height (atom nil)] (try (when (py-truthy (let [__b2 (py-lt radius-1 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt radius-2 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))))) (throw (ex-info (py-str "surface_area_conical_frustum() only accepts non-negative values") {:type :ValueError}))) (reset! slant-height (py-pow (py-add (py-pow height 2) (py-pow (py-sub radius-1 radius-2) 2)) 0.5)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul Math/PI (py-add (py-add (py-mul @slant-height (py-add radius-1 radius-2)) (py-pow radius-1 2)) (py-pow radius-2 2)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn surface-area-cylinder "Calculate the Surface Area of a Cylinder.\nWikipedia reference: https://en.wikipedia.org/wiki/Cylinder\nFormula: 2 * pi * r * (h + r)\n\n>>> surface_area_cylinder(7, 10)\n747.6990515543707\n>>> surface_area_cylinder(1.6, 2.6)\n42.22300526424682\n>>> surface_area_cylinder(0, 0)\n0.0\n>>> surface_area_cylinder(6, 8)\n527.7875658030853\n>>> surface_area_cylinder(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cylinder() only accepts non-negative values\n>>> surface_area_cylinder(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cylinder() only accepts non-negative values\n>>> surface_area_cylinder(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_cylinder() only accepts non-negative values"
  [radius height] (try (when (py-truthy (let [__b1 (py-lt radius 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))) (throw (ex-info (py-str "surface_area_cylinder() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-mul 2 Math/PI) radius) (py-add height radius))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn lateral-surface-cylinder "Calculate the Lateral Surface Area of a Cylinder.\nWikipedia reference: https://en.wikipedia.org/wiki/Cylinder\nFormula: 2 * pi * r * h\n\n>>> lateral_surface_cylinder(10, 20)\n1256.6370614359173\n>>> lateral_surface_cylinder(6, 8)\n301.59289474462014\n>>> lateral_surface_cylinder(0, 0)\n0.0\n>>> lateral_surface_cylinder(1, -2)\nTraceback (most recent call last):\n  ...\nValueError: lateral_surface_cylinder() only accepts non-negative values\n>>> lateral_surface_cylinder(-1, 2)\nTraceback (most recent call last):\n  ...\nValueError: lateral_surface_cylinder() only accepts non-negative values"
  [radius height] (try (when (py-truthy (let [__b1 (py-lt radius 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))) (throw (ex-info (py-str "lateral_surface_cylinder() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-mul 2 Math/PI) radius) height)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-torus "Calculate the Area of a Torus.\nWikipedia reference: https://en.wikipedia.org/wiki/Torus\n:return 4pi^2 * torus_radius * tube_radius\n>>> surface_area_torus(1, 1)\n39.47841760435743\n>>> surface_area_torus(4, 3)\n473.7410112522892\n>>> surface_area_torus(3, 4)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_torus() does not support spindle or self intersecting tori\n>>> surface_area_torus(1.6, 1.6)\n101.06474906715503\n>>> surface_area_torus(0, 0)\n0.0\n>>> surface_area_torus(-1, 1)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_torus() only accepts non-negative values\n>>> surface_area_torus(1, -1)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_torus() only accepts non-negative values"
  [torus-radius tube-radius] (try (when (py-truthy (let [__b1 (py-lt torus-radius 0)] (if (py-truthy __b1) __b1 (py-lt tube-radius 0)))) (throw (ex-info (py-str "surface_area_torus() only accepts non-negative values") {:type :ValueError}))) (when (py-truthy (py-lt torus-radius tube-radius)) (throw (ex-info (py-str "surface_area_torus() does not support spindle or self intersecting tori") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-mul 4 (py-pow Math/PI 2)) torus-radius) tube-radius)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-rectangle "Calculate the area of a rectangle.\n\n>>> area_rectangle(10, 20)\n200\n>>> area_rectangle(1.6, 2.6)\n4.16\n>>> area_rectangle(0, 0)\n0\n>>> area_rectangle(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_rectangle() only accepts non-negative values\n>>> area_rectangle(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_rectangle() only accepts non-negative values\n>>> area_rectangle(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: area_rectangle() only accepts non-negative values"
  [length width] (try (when (py-truthy (let [__b1 (py-lt length 0)] (if (py-truthy __b1) __b1 (py-lt width 0)))) (throw (ex-info (py-str "area_rectangle() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul length width)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-square "Calculate the area of a square.\n\n>>> area_square(10)\n100\n>>> area_square(0)\n0\n>>> area_square(1.6)\n2.5600000000000005\n>>> area_square(-1)\nTraceback (most recent call last):\n    ...\nValueError: area_square() only accepts non-negative values"
  [side-length] (try (when (py-truthy (py-lt side-length 0)) (throw (ex-info (py-str "area_square() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-pow side-length 2)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-triangle "Calculate the area of a triangle given the base and height.\n\n>>> area_triangle(10, 10)\n50.0\n>>> area_triangle(1.6, 2.6)\n2.08\n>>> area_triangle(0, 0)\n0.0\n>>> area_triangle(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_triangle() only accepts non-negative values\n>>> area_triangle(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_triangle() only accepts non-negative values\n>>> area_triangle(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: area_triangle() only accepts non-negative values"
  [base height] (try (when (py-truthy (let [__b1 (py-lt base 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))) (throw (ex-info (py-str "area_triangle() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul base height) 2)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-triangle-three-sides "Calculate area of triangle when the length of 3 sides are known.\nThis function uses Heron's formula: https://en.wikipedia.org/wiki/Heron%27s_formula\n\n>>> area_triangle_three_sides(5, 12, 13)\n30.0\n>>> area_triangle_three_sides(10, 11, 12)\n51.521233486786784\n>>> area_triangle_three_sides(0, 0, 0)\n0.0\n>>> area_triangle_three_sides(1.6, 2.6, 3.6)\n1.8703742940919619\n>>> area_triangle_three_sides(-1, -2, -1)\nTraceback (most recent call last):\n    ...\nValueError: area_triangle_three_sides() only accepts non-negative values\n>>> area_triangle_three_sides(1, -2, 1)\nTraceback (most recent call last):\n    ...\nValueError: area_triangle_three_sides() only accepts non-negative values\n>>> area_triangle_three_sides(2, 4, 7)\nTraceback (most recent call last):\n    ...\nValueError: Given three sides do not form a triangle\n>>> area_triangle_three_sides(2, 7, 4)\nTraceback (most recent call last):\n    ...\nValueError: Given three sides do not form a triangle\n>>> area_triangle_three_sides(7, 2, 4)\nTraceback (most recent call last):\n    ...\nValueError: Given three sides do not form a triangle"
  [side1 side2 side3] (let [area (atom nil) semi-perimeter (atom nil)] (try (when (py-truthy (let [__b2 (py-lt side1 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt side2 0)] (if (py-truthy __b1) __b1 (py-lt side3 0)))))) (throw (ex-info (py-str "area_triangle_three_sides() only accepts non-negative values") {:type :ValueError}))) (when (py-truthy (let [__b4 (py-lt (py-add side1 side2) side3)] (if (py-truthy __b4) __b4 (let [__b3 (py-lt (py-add side1 side3) side2)] (if (py-truthy __b3) __b3 (py-lt (py-add side2 side3) side1)))))) (throw (ex-info (py-str "Given three sides do not form a triangle") {:type :ValueError}))) (reset! semi-perimeter (py-div (py-add (py-add side1 side2) side3) 2)) (reset! area (Math/sqrt (py-mul (py-mul (py-mul @semi-perimeter (py-sub @semi-perimeter side1)) (py-sub @semi-perimeter side2)) (py-sub @semi-perimeter side3)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @area})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn area-parallelogram "Calculate the area of a parallelogram.\n\n>>> area_parallelogram(10, 20)\n200\n>>> area_parallelogram(1.6, 2.6)\n4.16\n>>> area_parallelogram(0, 0)\n0\n>>> area_parallelogram(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_parallelogram() only accepts non-negative values\n>>> area_parallelogram(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_parallelogram() only accepts non-negative values\n>>> area_parallelogram(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: area_parallelogram() only accepts non-negative values"
  [base height] (try (when (py-truthy (let [__b1 (py-lt base 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))) (throw (ex-info (py-str "area_parallelogram() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul base height)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-trapezium "Calculate the area of a trapezium.\n\n>>> area_trapezium(10, 20, 30)\n450.0\n>>> area_trapezium(1.6, 2.6, 3.6)\n7.5600000000000005\n>>> area_trapezium(0, 0, 0)\n0.0\n>>> area_trapezium(-1, -2, -3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values\n>>> area_trapezium(-1, 2, 3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values\n>>> area_trapezium(1, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values\n>>> area_trapezium(1, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values\n>>> area_trapezium(-1, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values\n>>> area_trapezium(1, -2, -3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values\n>>> area_trapezium(-1, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: area_trapezium() only accepts non-negative values"
  [base1 base2 height] (try (when (py-truthy (let [__b2 (py-lt base1 0)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt base2 0)] (if (py-truthy __b1) __b1 (py-lt height 0)))))) (throw (ex-info (py-str "area_trapezium() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-div 1 2) (py-add base1 base2)) height)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-circle "Calculate the area of a circle.\n\n>>> area_circle(20)\n1256.6370614359173\n>>> area_circle(1.6)\n8.042477193189871\n>>> area_circle(0)\n0.0\n>>> area_circle(-1)\nTraceback (most recent call last):\n    ...\nValueError: area_circle() only accepts non-negative values"
  [radius] (try (when (py-truthy (py-lt radius 0)) (throw (ex-info (py-str "area_circle() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul Math/PI (py-pow radius 2))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-ellipse "Calculate the area of a ellipse.\n\n>>> area_ellipse(10, 10)\n314.1592653589793\n>>> area_ellipse(10, 20)\n628.3185307179587\n>>> area_ellipse(0, 0)\n0.0\n>>> area_ellipse(1.6, 2.6)\n13.06902543893354\n>>> area_ellipse(-10, 20)\nTraceback (most recent call last):\n    ...\nValueError: area_ellipse() only accepts non-negative values\n>>> area_ellipse(10, -20)\nTraceback (most recent call last):\n    ...\nValueError: area_ellipse() only accepts non-negative values\n>>> area_ellipse(-10, -20)\nTraceback (most recent call last):\n    ...\nValueError: area_ellipse() only accepts non-negative values"
  [radius-x radius-y] (try (when (py-truthy (let [__b1 (py-lt radius-x 0)] (if (py-truthy __b1) __b1 (py-lt radius-y 0)))) (throw (ex-info (py-str "area_ellipse() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul Math/PI radius-x) radius-y)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-rhombus "Calculate the area of a rhombus.\n\n>>> area_rhombus(10, 20)\n100.0\n>>> area_rhombus(1.6, 2.6)\n2.08\n>>> area_rhombus(0, 0)\n0.0\n>>> area_rhombus(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_rhombus() only accepts non-negative values\n>>> area_rhombus(1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_rhombus() only accepts non-negative values\n>>> area_rhombus(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: area_rhombus() only accepts non-negative values"
  [diagonal-1 diagonal-2] (try (when (py-truthy (let [__b1 (py-lt diagonal-1 0)] (if (py-truthy __b1) __b1 (py-lt diagonal-2 0)))) (throw (ex-info (py-str "area_rhombus() only accepts non-negative values") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-mul (py-div 1 2) diagonal-1) diagonal-2)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn area-reg-polygon "Calculate the area of a regular polygon.\nWikipedia reference: https://en.wikipedia.org/wiki/Polygon#Regular_polygons\nFormula: (n*s^2*cot(pi/n))/4\n\n>>> area_reg_polygon(3, 10)\n43.301270189221945\n>>> area_reg_polygon(4, 10)\n100.00000000000001\n>>> area_reg_polygon(0, 0)\nTraceback (most recent call last):\n    ...\nValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides\n>>> area_reg_polygon(-1, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides\n>>> area_reg_polygon(5, -2)\nTraceback (most recent call last):\n    ...\nValueError: area_reg_polygon() only accepts non-negative values as length of a side\n>>> area_reg_polygon(-1, 2)\nTraceback (most recent call last):\n    ...\nValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides"
  [sides length] (try (when (py-truthy (let [__b1 (not (py-truthy (integer? sides)))] (if (py-truthy __b1) __b1 (py-lt sides 3)))) (throw (ex-info (py-str "area_reg_polygon() only accepts integers greater than or equal to three as number of sides") {:type :ValueError}))) (when (py-truthy (py-lt length 0)) (throw (ex-info (py-str "area_reg_polygon() only accepts non-negative values as length of a side") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (py-mul sides (py-pow length 2)) (py-mul 4 (Math/tan (py-div Math/PI sides))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn surface-area-reg-prism "Calculate the surface area of a regular prism.\nWikipedia reference: https://en.wikipedia.org/wiki/Prism\nFormula: 2*S(base) + n*a*h\n\n>>> surface_area_reg_prism(3, 10, 20)\n686.6025403784439\n>>> surface_area_reg_prism(4, 10, 10)\n600.0\n>>> surface_area_reg_prism(2, 10, 15)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_reg_prism() only accepts integers greater than or equal to three as number of sides\n>>> surface_area_reg_prism(5, -2, 3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_reg_prism() only accepts non-negative values as length of an edge or height\n>>> surface_area_reg_prism(5, 2, -3)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_reg_prism() only accepts non-negative values as length of an edge or height\n>>> surface_area_reg_prism(2, -10, 15)\nTraceback (most recent call last):\n    ...\nValueError: surface_area_reg_prism() only accepts integers greater than or equal to three as number of sides"
  [sides edge height] (try (when (py-truthy (let [__b1 (not (py-truthy (integer? sides)))] (if (py-truthy __b1) __b1 (py-lt sides 3)))) (throw (ex-info (py-str "surface_area_reg_prism() only accepts integers greater than or equal to three as number of sides") {:type :ValueError}))) (when (py-truthy (let [__b2 (py-lt edge 0)] (if (py-truthy __b2) __b2 (py-lt height 0)))) (throw (ex-info (py-str "surface_area_reg_prism() only accepts non-negative values as length of an edge or height") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-mul 2 (area-reg-polygon sides edge)) (py-mul (py-mul sides edge) height))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
