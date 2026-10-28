(ns clojure-algorithms.maths.ear-clipping-polygon-triangulation
  "An implementation of the ear clipping method for triangulating a simple polygon.\n\nWikipedia : https://en.wikipedia.org/wiki/Polygon_triangulation"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare is-ear is-convex cross-product direction is-point-inside-triangle triangulate-polygon)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-repeat-vec py-mul py-mod py-abs py-min py-min-by py-max py-max-by py-index py-float-str py-quote py-repr)

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

(defn- py-abs [x]
  (cond (boolean? x) (if x 1 0)
        (number? x) (if (neg? x) (-' x) x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :abs x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for abs()" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for abs()" {:type :TypeError}))))

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

(defn- py-index [c x]
  (cond (string? c) (let [i (.indexOf ^String c ^String (str x))]
                      (if (neg? i)
                        (throw (ex-info "substring not found" {:type :ValueError})) i))
        (sequential? c) (let [i (first (keep-indexed (fn [j e] (when (py-eq e x) j)) c))]
                          (if (nil? i)
                            (throw (ex-info "not in list" {:type :ValueError})) i))
        :else (throw (ex-info "no attribute 'index'" {:type :AttributeError}))))

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

(defn is-ear "This function determines whether three points form an ear.\n\n>>> is_ear([(0, 2), (2, 2), (2,0), (1, 0), (1, 1), (0, 1)], 4,\n... \"counter-clockwise\")\nFalse\n>>> is_ear([(0, 2), (2, 2), (2,0), (1, 0), (1, 1), (0, 1)], 3,\n... \"counter-clockwise\")\nTrue\n>>> is_ear([(0, 3), (2, 2), (3, 0), (3, 3)], 3, \"clockwise\")\nFalse\n>>> is_ear([(0, 0), (1, 0), (1, 1), (0, 1)], 0, \"clockwise\")\nTrue"
  [polygon point-idx direction] (let [j (atom nil) next-idx (atom nil) next-point (atom nil) point (atom nil) prev-idx (atom nil) prev-point (atom nil)] (try (reset! prev-idx (py-mod (py-sub point-idx 1) (count polygon))) (reset! next-idx (py-mod (py-add point-idx 1) (count polygon))) (reset! prev-point (py-get polygon @prev-idx)) (reset! point (py-get polygon point-idx)) (reset! next-point (py-get polygon @next-idx)) (when (py-truthy (is-convex @prev-point @point @next-point direction)) (doseq [__i1 (py-iter (range (count polygon)))] (reset! j __i1) (when (py-truthy (let [__b2 (not (py-in @j [@prev-idx point-idx @next-idx]))] (if (py-truthy __b2) (is-point-inside-triangle @prev-point @point @next-point (py-get polygon @j)) __b2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn is-convex "Determine with the ccw, if 3 points are convex.\n>>> is_convex((1,1), (2, 2), (1, 2), \"clockwise\")\nTrue\n>>> is_convex((1,1), (2, 2), (1, 2), \"counter-clockwise\")\nFalse\n>>> is_convex((1,1), (2, 2), (3, 3), \"clockwise\")\nTrue\n>>> is_convex((1,1), (2, 2), (3, 3), \"counter-clockwise\")\nTrue"
  [point prev-p next-p direction] (let [cross-product (atom nil)] (try (if (py-truthy (py-eq direction "counter-clockwise")) (do (reset! cross-product (py-sub (py-mul (py-sub (py-get next-p 0) (py-get point 0)) (py-sub (py-get prev-p 1) (py-get point 1))) (py-mul (py-sub (py-get prev-p 0) (py-get point 0)) (py-sub (py-get next-p 1) (py-get point 1)))))) (do (reset! cross-product (py-sub (py-mul (py-sub (py-get prev-p 0) (py-get point 0)) (py-sub (py-get next-p 1) (py-get point 1))) (py-mul (py-sub (py-get next-p 0) (py-get point 0)) (py-sub (py-get prev-p 1) (py-get point 1))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-ge @cross-product 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn cross-product "This function computes the product of two vectors, with 3 points.\nIf the vectors are collinear, the output is 0.\nIf the three points rotate counterclockwise, the output is positive.\nIf three points rotate clockwise, the output is negative.\n\n>>> cross_product((0, 0), (1, 0), (1.5, 0.5))\n0.5\n>>> cross_product((1.5, 0.5), (1, 1), (1.5, 1.5))\n-0.5\n>>> cross_product((0, 0), (1, 1), (2, 2))\n0"
  [p1 p2 p3] (py-sub (py-mul (py-sub (py-get p2 0) (py-get p1 0)) (py-sub (py-get p3 1) (py-get p1 1))) (py-mul (py-sub (py-get p2 1) (py-get p1 1)) (py-sub (py-get p3 0) (py-get p1 0)))))

(defn direction "Determine the orientation (clockwise or counterclockwise) of a polygon defined\nby a list of points.\n>>> direction([(1, 1), (2, 2), (3, 4)])\n'clockwise'\n>>> direction([(1, 1), (2, 2), (3, -1)])\n'counter-clockwise'"
  [polygon] (let [idx-p0 (atom nil) next-idx (atom nil) next-point (atom nil) point-0 (atom nil) prev-idx (atom nil) prev-point (atom nil)] (try (reset! point-0 (py-min-by (fn [point] [(py-get point 1) (py-get point 0)]) polygon)) (reset! idx-p0 (py-index polygon @point-0)) (reset! prev-idx (py-mod (py-sub @idx-p0 1) (count polygon))) (reset! next-idx (py-mod (py-add @idx-p0 1) (count polygon))) (reset! prev-point (py-get polygon @prev-idx)) (reset! next-point (py-get polygon @next-idx)) (while (py-truthy (py-eq (cross-product @point-0 @next-point @prev-point) 0)) (swap! next-idx py-add 1) (reset! next-point (py-get polygon @next-idx))) (when (py-truthy (py-gt (cross-product @point-0 @next-point @prev-point) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value "clockwise"}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value "counter-clockwise"})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn is-point-inside-triangle "Determine whether a given point is located inside a triangle\nformed by three other points.\n\nThis function calculates the area of both the triangle formed by\nthe three input points (p1, p2, p3)\nand the sub-triangles formed by replacing one vertex of the main\ntriangle with the test_point.\nIf the sum of the areas of the sub-triangles is equal to the area of\nthe main triangle, the test_point\nis considered to be inside the triangle. Otherwise, it is considered outside.\n\n>>> is_point_inside_triangle((0, 0), (0, 2), (2, 0), (3, 3))\nFalse\n>>> is_point_inside_triangle((0, 0), (0, 2), (2, 0), (1, 1))\nTrue\n>>> is_point_inside_triangle((0, 0), (2, 1), (2, 0), (1, 1))\nFalse\n>>> is_point_inside_triangle((0, 0), (2, 1), (2, 0), (2, 0))\nTrue\n>>> is_point_inside_triangle((0, 0), (1, 1), (2, 0), (1, 0))\nTrue"
  [p1 p2 p3 test-point] (let [area1 (atom nil) area2 (atom nil) area3 (atom nil) area-triangle (atom nil)] (try (reset! area-triangle (py-abs (py-mul 0.5 (py-add (py-add (py-mul (py-get p1 0) (py-sub (py-get p2 1) (py-get p3 1))) (py-mul (py-get p2 0) (py-sub (py-get p3 1) (py-get p1 1)))) (py-mul (py-get p3 0) (py-sub (py-get p1 1) (py-get p2 1))))))) (reset! area1 (py-abs (py-mul 0.5 (py-add (py-add (py-mul (py-get test-point 0) (py-sub (py-get p2 1) (py-get p3 1))) (py-mul (py-get p2 0) (py-sub (py-get p3 1) (py-get test-point 1)))) (py-mul (py-get p3 0) (py-sub (py-get test-point 1) (py-get p2 1))))))) (reset! area2 (py-abs (py-mul 0.5 (py-add (py-add (py-mul (py-get p1 0) (py-sub (py-get test-point 1) (py-get p3 1))) (py-mul (py-get test-point 0) (py-sub (py-get p3 1) (py-get p1 1)))) (py-mul (py-get p3 0) (py-sub (py-get p1 1) (py-get test-point 1))))))) (reset! area3 (py-abs (py-mul 0.5 (py-add (py-add (py-mul (py-get p1 0) (py-sub (py-get p2 1) (py-get test-point 1))) (py-mul (py-get p2 0) (py-sub (py-get test-point 1) (py-get p1 1)))) (py-mul (py-get test-point 0) (py-sub (py-get p1 1) (py-get p2 1))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @area-triangle (py-add (py-add @area1 @area2) @area3))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn triangulate-polygon "Triangulate a polygon and provide the points of the resulting triangles.\nThis function takes a list of coordinates that represent the vertices of a polygon.\nIt iteratively finds and removes 'ears' from the polygon to create a list\nof triangles that triangulate the entire polygon.\nThe order of vertices in the coordinates list is assumed to be\nconsistent (either clockwise or counterclockwise).\nThe function uses helper functions 'direction' to determine the polygon's\norientation and 'is_ear' to identify 'ear' vertices.\n\nNote: The function assumes that the input coordinates form a valid simple polygon.\n\n>>> triangulate_polygon([(0, 2), (2, 2), (2, 0), (1, 0), (1, 1), (0, 1)])\n[[(0, 1), (0, 2), (2, 2)], [(2, 2), (2, 0), (1, 0)], [(2, 2), (1, 0), (1, 1)], [(0, 1), (2, 2), (1, 1)]]\n>>> triangulate_polygon([(0, 3), (2, 2), (3, 0), (3, 3)])\n[[(3, 3), (0, 3), (2, 2)], [(3, 3), (2, 2), (3, 0)]]\n>>> triangulate_polygon([(0, 0),(2, 0), (1, 1), (2, 2), (0,2)])\n[[(0, 0), (2, 0), (1, 1)], [(0, 2), (0, 0), (1, 1)], [(0, 2), (1, 1), (2, 2)]]"
  [& _args]
  ;; stub: original requires numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:copy" {:type :UnsupportedOperation})))

;; NOTE: triangulate_polygon stubbed (numpy-attr:copy)
