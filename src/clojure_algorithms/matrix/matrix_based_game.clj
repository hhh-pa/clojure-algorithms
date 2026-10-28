(ns clojure-algorithms.matrix.matrix-based-game
  "Matrix-Based Game Script\n=========================\nThis script implements a matrix-based game where players interact with a grid of\nelements. The primary goals are to:\n- Identify connected elements of the same type from a selected position.\n- Remove those elements, adjust the matrix by simulating gravity, and reorganize empty\n  columns.\n- Calculate and display the score based on the number of elements removed in each move.\n\nFunctions:\n-----------\n1. `find_repeat`: Finds all connected elements of the same type.\n2. `increment_score`: Calculates the score for a given move.\n3. `move_x`: Simulates gravity in a column.\n4. `move_y`: Reorganizes the matrix by shifting columns leftward when a column becomes\n    empty.\n5. `play`: Executes a single move, updating the matrix and returning the score.\n\nInput Format:\n--------------\n1. Matrix size (`lines`): Integer specifying the size of the matrix (N x N).\n2. Matrix content (`matrix`): Rows of the matrix, each consisting of characters.\n3. Number of moves (`movs`): Integer indicating the number of moves.\n4. List of moves (`movements`): A comma-separated string of coordinates for each move.\n\n(0,0) position starts from first left column to last right, and below row to up row\n\n\nExample Input:\n---------------\n4\nRRBG\nRBBG\nYYGG\nXYGG\n2\n0 1,1 1\n\nExample (0,0) = X\n\nOutput:\n--------\nThe script outputs the total score after processing all moves.\n\nUsage:\n-------\nRun the script and provide the required inputs as prompted."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare validate-matrix-size validate-matrix-content validate-moves parse-moves find-repeat increment-score move-x move-y play process-game)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-div py-neg py-list py-set py-insert py-float-str py-quote py-repr py-str py-int py-strip py-split)

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

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-insert [c i x]
  (if (or (sequential? c) (nil? c))
    (let [v (vec c) n (count v)
          i (cond (boolean? i) (if i 1 0)
                  (integer? i) (long i)
                  :else (throw (ex-info "indices must be integers" {:type :TypeError})))
          ii (cond (neg? i) (max 0 (+ n i)) (> i n) n :else i)]
      (vec (concat (subvec v 0 ii) [x] (subvec v ii))))
    (throw (ex-info "no attribute 'insert'" {:type :AttributeError}))))

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

(defn- py-strip [s chars side]
  (if (nil? chars)
    (case side :left (clojure.string/replace s #"^\s+" "") :right (clojure.string/replace s #"\s+$" "") (clojure.string/trim s))
    (let [cs (set (seq chars))
          drop-l (fn [sq] (drop-while cs sq))
          drop-r (fn [sq] (reverse (drop-while cs (reverse sq))))]
      (apply str (case side :left (drop-l (seq s)) :right (drop-r (seq s)) (drop-r (drop-l (seq s))))))))

(defn- py-split
  ([s] (py-split s nil -1))
  ([s sep] (py-split s sep -1))
  ([s sep mx]
   (let [s (str s)]
     (if (nil? sep)
       (let [parts (vec (remove empty? (clojure.string/split s #"\s+")))]
         (if (or (neg? mx) (>= mx (count parts))) parts
             (let [head (clojure.string/join " " (take (- (count parts) mx) parts))]
               (vec (concat [head] (take-last mx parts))))))
       (do (when (empty? sep) (throw (ex-info "empty separator" {:type :ValueError})))
           (let [re (re-pattern (java.util.regex.Pattern/quote sep))]
             (if (neg? mx) (vec (.split ^String s (.pattern re) -1))
                 (vec (.split ^String s (.pattern re) (int (inc mx)))))))))))

(defn validate-matrix-size ">>> validate_matrix_size(-1)\nTraceback (most recent call last):\n    ...\nValueError: Matrix size must be a positive integer."
  [size] (when (py-truthy (let [__b1 (not (py-truthy (integer? size)))] (if (py-truthy __b1) __b1 (py-le size 0)))) (throw (ex-info (py-str "Matrix size must be a positive integer.") {:type :ValueError}))) nil)

(defn validate-matrix-content "Validates that the number of elements in the matrix matches the given size.\n\n>>> validate_matrix_content(['aaaa', 'aaaa', 'aaaa', 'aaaa'], 3)\nTraceback (most recent call last):\n    ...\nValueError: The matrix dont match with size.\n>>> validate_matrix_content(['aa%', 'aaa', 'aaa'], 3)\nTraceback (most recent call last):\n    ...\nValueError: Matrix rows can only contain letters and numbers.\n>>> validate_matrix_content(['aaa', 'aaa', 'aaaa'], 3)\nTraceback (most recent call last):\n    ...\nValueError: Each row in the matrix must have exactly 3 characters."
  [matrix size] (let [msg (atom nil) row (atom nil)] (println (clojure.string/join " " [(py-str matrix)])) (when (py-truthy (not (py-eq (count matrix) size))) (throw (ex-info (py-str "The matrix dont match with size.") {:type :ValueError}))) (doseq [__i1 (py-iter matrix)] (reset! row __i1) (when (py-truthy (not (py-eq (count @row) size))) (reset! msg (str "Each row in the matrix must have exactly " (py-str size) " characters.")) (throw (ex-info (py-str @msg) {:type :ValueError}))) (when (py-truthy (not (py-truthy (every? py-truthy (doall (for [char (py-iter @row)] (boolean (re-matches #"[\p{L}\p{N}]+" char)))))))) (throw (ex-info (py-str "Matrix rows can only contain letters and numbers.") {:type :ValueError})))) nil))

(defn validate-moves ">>> validate_moves([(1, 2), (-1, 0)], 3)\nTraceback (most recent call last):\n    ...\nValueError: Move is out of bounds for a matrix."
  [moves size] (let [move (atom nil) x (atom nil) y (atom nil)] (doseq [__i1 (py-iter moves)] (reset! move __i1) (let [__d2 (vec @move)] (reset! x (nth __d2 0)) (reset! y (nth __d2 1))) (when (py-truthy (not (py-truthy (let [__b9 (let [__c3 0 __c4 @x __c5 size] (and (py-le __c3 __c4) (py-lt __c4 __c5)))] (if (py-truthy __b9) (let [__c6 0 __c7 @y __c8 size] (and (py-le __c6 __c7) (py-lt __c7 __c8))) __b9))))) (throw (ex-info (py-str "Move is out of bounds for a matrix.") {:type :ValueError})))) nil))

(defn parse-moves ">>> parse_moves(\"0 1, 1 1\")\n[(0, 1), (1, 1)]\n>>> parse_moves(\"0 1, 1 1, 2\")\nTraceback (most recent call last):\n    ...\nValueError: Each move must have exactly two numbers.\n>>> parse_moves(\"0 1, 1 1, 2 4 5 6\")\nTraceback (most recent call last):\n    ...\nValueError: Each move must have exactly two numbers."
  [input-str] (let [moves (atom nil) pair (atom nil) parts (atom nil) x (atom nil) y (atom nil)] (try (reset! moves []) (doseq [__i1 (py-iter (py-split input-str "," -1))] (reset! pair __i1) (reset! parts (py-split (py-strip @pair nil :both))) (when (py-truthy (not (py-eq (count @parts) 2))) (throw (ex-info (py-str "Each move must have exactly two numbers.") {:type :ValueError}))) (let [__d2 (vec (map (fn [x] (py-int x nil)) @parts))] (reset! x (nth __d2 0)) (reset! y (nth __d2 1))) (swap! moves conj [@x @y])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @moves})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn find-repeat "Finds all connected elements of the same type from a given position.\n\n>>> find_repeat([['A', 'B', 'A'], ['A', 'B', 'A'], ['A', 'A', 'A']], 0, 0, 3)\n{(1, 2), (2, 1), (0, 0), (2, 0), (0, 2), (2, 2), (1, 0)}\n>>> find_repeat([['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']], 1, 1, 3)\nset()"
  [matrix-g row column size] (let [column (atom column)] (let [color (atom nil) repeated (atom nil) visited (atom nil)] (try (reset! column (py-sub (py-sub size 1) @column)) (reset! visited (py-set [])) (reset! repeated (py-set [])) (when (py-truthy (not (py-eq (do (reset! color (py-get (py-get matrix-g @column) row)) @color) "-"))) (letfn [(dfs [row-n column-n] (let [repeated (atom nil) visited (atom nil)] (try (when (py-truthy (let [__b3 (py-lt row-n 0)] (if (py-truthy __b3) __b3 (let [__b2 (py-ge row-n size)] (if (py-truthy __b2) __b2 (let [__b1 (py-lt column-n 0)] (if (py-truthy __b1) __b1 (py-ge column-n size)))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (when (py-truthy (py-in [row-n column-n] @visited)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (swap! visited (fn [__c] (conj (set __c) [row-n column-n]))) (when (py-truthy (py-eq (py-get (py-get matrix-g row-n) column-n) @color)) (swap! repeated (fn [__c] (conj (set __c) [row-n column-n]))) (dfs (py-sub row-n 1) column-n) (dfs (py-add row-n 1) column-n) (dfs row-n (py-sub column-n 1)) (dfs row-n (py-add column-n 1))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))] (dfs @column row))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @repeated})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn increment-score "Calculates the score for a move based on the number of elements removed.\n\n>>> increment_score(3)\n6\n>>> increment_score(0)\n0"
  [count-v] (py-int (py-div (py-mul count-v (py-add count-v 1)) 2) nil))

(defn move-x "Simulates gravity in a specific column.\n\n>>> move_x([['-', 'A'], ['-', '-'], ['-', 'C']], 1, 2)\n[['-', '-'], ['-', 'A'], ['-', 'C']]"
  [matrix-g column size] (let [matrix-g (atom matrix-g)] (let [new-list (atom nil) row (atom nil)] (try (reset! new-list []) (doseq [__i1 (py-iter (range size))] (reset! row __i1) (if (py-truthy (not (py-eq (py-get (py-get @matrix-g @row) column) "-"))) (do (swap! new-list conj (py-get (py-get @matrix-g @row) column))) (do (swap! new-list py-insert 0 (py-get (py-get @matrix-g @row) column))))) (doseq [__i2 (py-iter (range size))] (reset! row __i2) (swap! matrix-g (fn [__c] (py-assoc-in __c [@row column] (py-get @new-list @row))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix-g})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn move-y "Shifts all columns leftward when an entire column becomes empty.\n\n>>> move_y([['-', 'A'], ['-', '-'], ['-', 'C']], 2)\n[['A', '-'], ['-', '-'], ['-', 'C']]"
  [matrix-g size] (let [matrix-g (atom matrix-g)] (let [col (atom nil) column (atom nil) empty-columns (atom nil) row (atom nil)] (try (reset! empty-columns []) (doseq [__i1 (py-iter (range (py-sub size 1) (py-neg 1) (py-neg 1)))] (reset! column __i1) (when (py-truthy (every? py-truthy (doall (for [row (py-iter (range size))] (py-eq (py-get (py-get @matrix-g row) @column) "-"))))) (swap! empty-columns conj @column))) (doseq [__i2 (py-iter @empty-columns)] (reset! column __i2) (doseq [__i3 (py-iter (range (py-add @column 1) size))] (reset! col __i3) (doseq [__i4 (py-iter (range size))] (reset! row __i4) (swap! matrix-g (fn [__c] (py-assoc-in __c [@row (py-sub @col 1)] (py-get (py-get @matrix-g @row) @col)))))) (doseq [__i5 (py-iter (range size))] (reset! row __i5) (swap! matrix-g (fn [__c] (py-assoc-in __c [@row (py-neg 1)] "-"))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix-g})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn play "Processes a single move, updating the matrix and calculating the score.\n\n>>> play([['R', 'G'], ['R', 'G']], 0, 0, 2)\n([['G', '-'], ['G', '-']], 3)"
  [matrix-g pos-x pos-y size] (let [matrix-g (atom matrix-g)] (let [column (atom nil) pos (atom nil) same-colors (atom nil)] (try (reset! same-colors (find-repeat @matrix-g pos-x pos-y size)) (when (py-truthy (not (py-eq (count @same-colors) 0))) (doseq [__i1 (py-iter @same-colors)] (reset! pos __i1) (swap! matrix-g (fn [__c] (py-assoc-in __c [(py-get @pos 0) (py-get @pos 1)] "-")))) (doseq [__i2 (py-iter (range size))] (reset! column __i2) (reset! matrix-g (move-x @matrix-g @column size))) (reset! matrix-g (move-y @matrix-g size))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@matrix-g (increment-score (count @same-colors))]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn process-game "Processes the game logic for the given matrix and moves.\n\nArgs:\n    size (int): Size of the game board.\n    matrix (List[str]): Initial game matrix.\n    moves (List[Tuple[int, int]]): List of moves as (x, y) coordinates.\n\nReturns:\n    int: The total score obtained.\n>>> process_game(3, ['aaa', 'bbb', 'ccc'], [(0, 0)])\n6"
  [size matrix moves] (let [game-matrix (atom nil) move (atom nil) pos-x (atom nil) pos-y (atom nil) score (atom nil) total-score (atom nil)] (try (reset! game-matrix (vec (for [row (py-iter matrix)] (py-list row)))) (reset! total-score 0) (doseq [__i1 (py-iter moves)] (reset! move __i1) (let [__d2 (vec @move)] (reset! pos-x (nth __d2 0)) (reset! pos-y (nth __d2 1))) (let [__d3 (vec (play @game-matrix @pos-x @pos-y size))] (reset! game-matrix (nth __d3 0)) (reset! score (nth __d3 1))) (swap! total-score py-add @score)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @total-score})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
