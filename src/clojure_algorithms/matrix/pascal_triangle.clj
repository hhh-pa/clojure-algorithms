(ns clojure-algorithms.matrix.pascal-triangle
  "This implementation demonstrates how to generate the elements of a Pascal's triangle.\nThe element havingva row index of r and column index of c can be derivedvas follows:\ntriangle[r][c] = triangle[r-1][c-1]+triangle[r-1][c]\n\nA Pascal's triangle is a triangular array containing binomial coefficients.\nhttps://en.wikipedia.org/wiki/Pascal%27s_triangle"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare print-pascal-triangle generate-pascal-triangle populate-current-row calculate-current-element generate-pascal-triangle-optimized benchmark)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-slice py-add py-sub py-repeat-vec py-mul py-floordiv py-mod py-neg py-float-str py-quote py-repr py-str)

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

(defn print-pascal-triangle "Print Pascal's triangle for different number of rows\n>>> print_pascal_triangle(5)\n    1\n   1 1\n  1 2 1\n 1 3 3 1\n1 4 6 4 1"
  [num-rows] (let [--v (atom nil) col-idx (atom nil) row-idx (atom nil) triangle (atom nil)] (reset! triangle (generate-pascal-triangle num-rows)) (doseq [__i1 (py-iter (range num-rows))] (reset! row-idx __i1) (doseq [__i2 (py-iter (range (py-sub (py-sub num-rows @row-idx) 1)))] (reset! --v __i2) (print " ")) (doseq [__i3 (py-iter (range (py-add @row-idx 1)))] (reset! col-idx __i3) (if (py-truthy (not (py-eq @col-idx @row-idx))) (do (print (str (clojure.string/join " " [(py-str (py-get (py-get @triangle @row-idx) @col-idx))]) " "))) (do (print (str (clojure.string/join " " [(py-str (py-get (py-get @triangle @row-idx) @col-idx))]) ""))))) (print "\n")) nil))

(defn generate-pascal-triangle "Create Pascal's triangle for different number of rows\n>>> generate_pascal_triangle(0)\n[]\n>>> generate_pascal_triangle(1)\n[[1]]\n>>> generate_pascal_triangle(2)\n[[1], [1, 1]]\n>>> generate_pascal_triangle(3)\n[[1], [1, 1], [1, 2, 1]]\n>>> generate_pascal_triangle(4)\n[[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]]\n>>> generate_pascal_triangle(5)\n[[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]\n>>> generate_pascal_triangle(-5)\nTraceback (most recent call last):\n    ...\nValueError: The input value of 'num_rows' should be greater than or equal to 0\n>>> generate_pascal_triangle(7.89)\nTraceback (most recent call last):\n    ...\nTypeError: The input value of 'num_rows' should be 'int'"
  [num-rows] (let [current-row (atom nil) current-row-idx (atom nil) triangle (atom nil)] (try (when (py-truthy (not (py-truthy (integer? num-rows)))) (throw (ex-info (py-str "The input value of 'num_rows' should be 'int'") {:type :TypeError}))) (if (py-truthy (py-eq num-rows 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (do (when (py-truthy (py-lt num-rows 0)) (throw (ex-info (py-str "The input value of 'num_rows' should be greater than or equal to 0") {:type :ValueError}))))) (reset! triangle []) (doseq [__i1 (py-iter (range num-rows))] (reset! current-row-idx __i1) (reset! current-row (populate-current-row @triangle @current-row-idx)) (swap! triangle conj @current-row)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @triangle})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn populate-current-row ">>> triangle = [[1]]\n>>> populate_current_row(triangle, 1)\n[1, 1]"
  [triangle current-row-idx] (let [current-col-idx (atom nil) current-row (atom nil)] (try (reset! current-row (py-mul [(py-neg 1)] (py-add current-row-idx 1))) (let [__d1 (vec [1 1])] (swap! current-row (fn [__c] (py-assoc-in __c [0] (nth __d1 0)))) (swap! current-row (fn [__c] (py-assoc-in __c [(py-neg 1)] (nth __d1 1))))) (doseq [__i2 (py-iter (range 1 current-row-idx))] (reset! current-col-idx __i2) (calculate-current-element triangle @current-row current-row-idx @current-col-idx)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @current-row})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn calculate-current-element ">>> triangle = [[1], [1, 1]]\n>>> current_row = [1, -1, 1]\n>>> calculate_current_element(triangle, current_row, 2, 1)\n>>> current_row\n[1, 2, 1]"
  [triangle current-row current-row-idx current-col-idx] (let [current-row (atom current-row)] (let [above-to-left-elt (atom nil) above-to-right-elt (atom nil)] (reset! above-to-left-elt (py-get (py-get triangle (py-sub current-row-idx 1)) (py-sub current-col-idx 1))) (reset! above-to-right-elt (py-get (py-get triangle (py-sub current-row-idx 1)) current-col-idx)) (swap! current-row (fn [__c] (py-assoc-in __c [current-col-idx] (py-add @above-to-left-elt @above-to-right-elt)))) nil)))

(defn generate-pascal-triangle-optimized "This function returns a matrix representing the corresponding pascal's triangle\naccording to the given input of number of rows of Pascal's triangle to be generated.\nIt reduces the operations done to generate a row by half\nby eliminating redundant calculations.\n\n:param num_rows: Integer specifying the number of rows in the Pascal's triangle\n:return: 2-D List (matrix) representing the Pascal's triangle\n\nReturn the Pascal's triangle of given rows\n>>> generate_pascal_triangle_optimized(3)\n[[1], [1, 1], [1, 2, 1]]\n>>> generate_pascal_triangle_optimized(1)\n[[1]]\n>>> generate_pascal_triangle_optimized(0)\n[]\n>>> generate_pascal_triangle_optimized(-5)\nTraceback (most recent call last):\n    ...\nValueError: The input value of 'num_rows' should be greater than or equal to 0\n>>> generate_pascal_triangle_optimized(7.89)\nTraceback (most recent call last):\n    ...\nTypeError: The input value of 'num_rows' should be 'int'"
  [num-rows] (let [distinct-elements (atom nil) result (atom nil) row (atom nil) row-first-half (atom nil) row-index (atom nil) row-length (atom nil) row-second-half (atom nil) temp-row (atom nil)] (try (when (py-truthy (not (py-truthy (integer? num-rows)))) (throw (ex-info (py-str "The input value of 'num_rows' should be 'int'") {:type :TypeError}))) (if (py-truthy (py-eq num-rows 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (do (when (py-truthy (py-lt num-rows 0)) (throw (ex-info (py-str "The input value of 'num_rows' should be greater than or equal to 0") {:type :ValueError}))))) (reset! result [[1]]) (doseq [__i1 (py-iter (range 1 num-rows))] (reset! row-index __i1) (reset! temp-row (py-add (py-add [0] (py-get @result (py-neg 1))) [0])) (reset! row-length (py-add @row-index 1)) (reset! distinct-elements (reduce +' 0 [(py-floordiv @row-length 2) (py-mod @row-length 2)])) (reset! row-first-half (vec (for [i (py-iter (range 1 (py-add @distinct-elements 1)))] (py-add (py-get @temp-row (py-sub i 1)) (py-get @temp-row i))))) (reset! row-second-half (py-slice @row-first-half nil (py-floordiv (py-add @row-index 1) 2) nil)) (reset! row-second-half (vec (reverse @row-second-half))) (reset! row (py-add @row-first-half @row-second-half)) (swap! result conj @row)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn benchmark "Benchmark multiple functions, with three different length int values."
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

;; NOTE: benchmark stubbed (timeit)
