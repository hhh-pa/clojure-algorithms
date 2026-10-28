(ns clojure-algorithms.matrix.largest-square-area-in-matrix
  "Question:\nGiven a binary matrix mat of size n * m, find out the maximum size square\nsub-matrix with all 1s.\n\n---\nExample 1:\n\nInput:\nn = 2, m = 2\nmat = [[1, 1],\n       [1, 1]]\n\nOutput:\n2\n\nExplanation: The maximum size of the square\nsub-matrix is 2. The matrix itself is the\nmaximum sized sub-matrix in this case.\n---\nExample 2\n\nInput:\nn = 2, m = 2\nmat = [[0, 0],\n       [0, 0]]\nOutput: 0\n\nExplanation: There is no 1 in the matrix.\n\n\nApproach:\nWe initialize another matrix (dp) with the same dimensions\nas the original one initialized with all 0's.\n\ndp_array(i,j) represents the side length of the maximum square whose\nbottom right corner is the cell with index (i,j) in the original matrix.\n\nStarting from index (0,0), for every 1 found in the original matrix,\nwe update the value of the current element as\n\ndp_array(i,j)=dp_array(dp(i-1,j),dp_array(i-1,j-1),dp_array(i,j-1)) + 1."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare largest-square-area-in-matrix-top-down-approch largest-square-area-in-matrix-top-down-approch-with-dp largest-square-area-in-matrix-bottom-up largest-square-area-in-matrix-bottom-up-space-optimization)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-neg py-min py-min-by py-max py-max-by py-float-str py-quote py-repr)

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

(defn largest-square-area-in-matrix-top-down-approch "Function updates the largest_square_area[0], if recursive call found\nsquare with maximum area.\n\nWe aren't using dp_array here, so the time complexity would be exponential.\n\n>>> largest_square_area_in_matrix_top_down_approch(2, 2, [[1,1], [1,1]])\n2\n>>> largest_square_area_in_matrix_top_down_approch(2, 2, [[0,0], [0,0]])\n0"
  [rows cols mat] (let [largest-square-area (atom nil)] (try (letfn [(update-area-of-max-square [row col] (let [diagonal (atom nil) down (atom nil) largest-square-area (atom nil) right (atom nil) sub-problem-sol (atom nil)] (try (when (py-truthy (let [__b1 (py-ge row rows)] (if (py-truthy __b1) __b1 (py-ge col cols)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (reset! right (update-area-of-max-square row (py-add col 1))) (reset! diagonal (update-area-of-max-square (py-add row 1) (py-add col 1))) (reset! down (update-area-of-max-square (py-add row 1) col)) (if (py-truthy (py-get (py-get mat row) col)) (do (reset! sub-problem-sol (py-add 1 (py-min [@right @diagonal @down]))) (swap! largest-square-area (fn [__c] (py-assoc-in __c [0] (py-max [(py-get @largest-square-area 0) @sub-problem-sol])))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @sub-problem-sol}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))] (reset! largest-square-area [0]) (update-area-of-max-square 0 0) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @largest-square-area 0)}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn largest-square-area-in-matrix-top-down-approch-with-dp "Function updates the largest_square_area[0], if recursive call found\nsquare with maximum area.\n\nWe are using dp_array here, so the time complexity would be O(N^2).\n\n>>> largest_square_area_in_matrix_top_down_approch_with_dp(2, 2, [[1,1], [1,1]])\n2\n>>> largest_square_area_in_matrix_top_down_approch_with_dp(2, 2, [[0,0], [0,0]])\n0"
  [rows cols mat] (let [dp-array (atom nil) largest-square-area (atom nil)] (try (letfn [(update-area-of-max-square-using-dp-array [row col dp-array] (let [dp-array (atom dp-array)] (let [diagonal (atom nil) down (atom nil) largest-square-area (atom nil) right (atom nil) sub-problem-sol (atom nil)] (try (when (py-truthy (let [__b1 (py-ge row rows)] (if (py-truthy __b1) __b1 (py-ge col cols)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (when (py-truthy (not (py-eq (py-get (py-get @dp-array row) col) (py-neg 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get @dp-array row) col)}))) (reset! right (update-area-of-max-square-using-dp-array row (py-add col 1) @dp-array)) (reset! diagonal (update-area-of-max-square-using-dp-array (py-add row 1) (py-add col 1) @dp-array)) (reset! down (update-area-of-max-square-using-dp-array (py-add row 1) col @dp-array)) (if (py-truthy (py-get (py-get mat row) col)) (do (reset! sub-problem-sol (py-add 1 (py-min [@right @diagonal @down]))) (swap! largest-square-area (fn [__c] (py-assoc-in __c [0] (py-max [(py-get @largest-square-area 0) @sub-problem-sol])))) (swap! dp-array (fn [__c] (py-assoc-in __c [row col] @sub-problem-sol))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @sub-problem-sol}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))] (reset! largest-square-area [0]) (reset! dp-array (vec (for [--v (py-iter (range rows))] (py-mul [(py-neg 1)] cols)))) (update-area-of-max-square-using-dp-array 0 0 @dp-array) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @largest-square-area 0)}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn largest-square-area-in-matrix-bottom-up "Function updates the largest_square_area, using bottom up approach.\n\n>>> largest_square_area_in_matrix_bottom_up(2, 2, [[1,1], [1,1]])\n2\n>>> largest_square_area_in_matrix_bottom_up(2, 2, [[0,0], [0,0]])\n0"
  [rows cols mat] (let [bottom (atom nil) col (atom nil) diagonal (atom nil) dp-array (atom nil) largest-square-area (atom nil) right (atom nil) row (atom nil)] (try (reset! dp-array (vec (for [--v (py-iter (range (py-add rows 1)))] (py-mul [0] (py-add cols 1))))) (reset! largest-square-area 0) (doseq [__i1 (py-iter (range (py-sub rows 1) (py-neg 1) (py-neg 1)))] (reset! row __i1) (doseq [__i2 (py-iter (range (py-sub cols 1) (py-neg 1) (py-neg 1)))] (reset! col __i2) (reset! right (py-get (py-get @dp-array @row) (py-add @col 1))) (reset! diagonal (py-get (py-get @dp-array (py-add @row 1)) (py-add @col 1))) (reset! bottom (py-get (py-get @dp-array (py-add @row 1)) @col)) (if (py-truthy (py-eq (py-get (py-get mat @row) @col) 1)) (do (swap! dp-array (fn [__c] (py-assoc-in __c [@row @col] (py-add 1 (py-min [@right @diagonal @bottom]))))) (reset! largest-square-area (py-max [(py-get (py-get @dp-array @row) @col) @largest-square-area]))) (do (swap! dp-array (fn [__c] (py-assoc-in __c [@row @col] 0))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @largest-square-area})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn largest-square-area-in-matrix-bottom-up-space-optimization "Function updates the largest_square_area, using bottom up\napproach. with space optimization.\n\n>>> largest_square_area_in_matrix_bottom_up_space_optimization(2, 2, [[1,1], [1,1]])\n2\n>>> largest_square_area_in_matrix_bottom_up_space_optimization(2, 2, [[0,0], [0,0]])\n0"
  [rows cols mat] (let [bottom (atom nil) col (atom nil) current-row (atom nil) diagonal (atom nil) largest-square-area (atom nil) next-row (atom nil) right (atom nil) row (atom nil)] (try (reset! current-row (py-mul [0] (py-add cols 1))) (reset! next-row (py-mul [0] (py-add cols 1))) (reset! largest-square-area 0) (doseq [__i1 (py-iter (range (py-sub rows 1) (py-neg 1) (py-neg 1)))] (reset! row __i1) (doseq [__i2 (py-iter (range (py-sub cols 1) (py-neg 1) (py-neg 1)))] (reset! col __i2) (reset! right (py-get @current-row (py-add @col 1))) (reset! diagonal (py-get @next-row (py-add @col 1))) (reset! bottom (py-get @next-row @col)) (if (py-truthy (py-eq (py-get (py-get mat @row) @col) 1)) (do (swap! current-row (fn [__c] (py-assoc-in __c [@col] (py-add 1 (py-min [@right @diagonal @bottom]))))) (reset! largest-square-area (py-max [(py-get @current-row @col) @largest-square-area]))) (do (swap! current-row (fn [__c] (py-assoc-in __c [@col] 0)))))) (reset! next-row @current-row)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @largest-square-area})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
