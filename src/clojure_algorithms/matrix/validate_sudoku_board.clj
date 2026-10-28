(ns clojure-algorithms.matrix.validate-sudoku-board
  "LeetCode 36. Valid Sudoku\nhttps://leetcode.com/problems/valid-sudoku/\nhttps://en.wikipedia.org/wiki/Sudoku\n\nDetermine if a 9 x 9 Sudoku board is valid. Only the filled cells need to be\nvalidated according to the following rules:\n\n- Each row must contain the digits 1-9 without repetition.\n- Each column must contain the digits 1-9 without repetition.\n- Each of the nine 3 x 3 sub-boxes of the grid must contain the digits 1-9\n  without repetition.\n\nNote:\n\nA Sudoku board (partially filled) could be valid but is not necessarily\nsolvable.\n\nOnly the filled cells need to be validated according to the mentioned rules."
  (:require [clojure.string :as str]))

(declare is-valid-sudoku-board NUM-SQUARES EMPTY-CELL)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-ddget py-floordiv py-float-str py-quote py-repr py-str)

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

(defn- py-ddget [c k d]
  (if (map? c) (get c k d) (py-get c k)))

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

(defn is-valid-sudoku-board "This function validates (but does not solve) a sudoku board.\nThe board may be valid but unsolvable.\n\n>>> is_valid_sudoku_board([\n...  [\"5\",\"3\",\".\",\".\",\"7\",\".\",\".\",\".\",\".\"]\n... ,[\"6\",\".\",\".\",\"1\",\"9\",\"5\",\".\",\".\",\".\"]\n... ,[\".\",\"9\",\"8\",\".\",\".\",\".\",\".\",\"6\",\".\"]\n... ,[\"8\",\".\",\".\",\".\",\"6\",\".\",\".\",\".\",\"3\"]\n... ,[\"4\",\".\",\".\",\"8\",\".\",\"3\",\".\",\".\",\"1\"]\n... ,[\"7\",\".\",\".\",\".\",\"2\",\".\",\".\",\".\",\"6\"]\n... ,[\".\",\"6\",\".\",\".\",\".\",\".\",\"2\",\"8\",\".\"]\n... ,[\".\",\".\",\".\",\"4\",\"1\",\"9\",\".\",\".\",\"5\"]\n... ,[\".\",\".\",\".\",\".\",\"8\",\".\",\".\",\"7\",\"9\"]\n... ])\nTrue\n>>> is_valid_sudoku_board([\n...  [\"8\",\"3\",\".\",\".\",\"7\",\".\",\".\",\".\",\".\"]\n... ,[\"6\",\".\",\".\",\"1\",\"9\",\"5\",\".\",\".\",\".\"]\n... ,[\".\",\"9\",\"8\",\".\",\".\",\".\",\".\",\"6\",\".\"]\n... ,[\"8\",\".\",\".\",\".\",\"6\",\".\",\".\",\".\",\"3\"]\n... ,[\"4\",\".\",\".\",\"8\",\".\",\"3\",\".\",\".\",\"1\"]\n... ,[\"7\",\".\",\".\",\".\",\"2\",\".\",\".\",\".\",\"6\"]\n... ,[\".\",\"6\",\".\",\".\",\".\",\".\",\"2\",\"8\",\".\"]\n... ,[\".\",\".\",\".\",\"4\",\"1\",\"9\",\".\",\".\",\"5\"]\n... ,[\".\",\".\",\".\",\".\",\"8\",\".\",\".\",\"7\",\"9\"]\n... ])\nFalse\n>>> is_valid_sudoku_board([\n...  [\"1\",\"2\",\"3\",\"4\",\"5\",\"6\",\"7\",\"8\",\"9\"]\n... ,[\"4\",\"5\",\"6\",\"7\",\"8\",\"9\",\"1\",\"2\",\"3\"]\n... ,[\"7\",\"8\",\"9\",\"1\",\"2\",\"3\",\"4\",\"5\",\"6\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ])\nTrue\n>>> is_valid_sudoku_board([\n...  [\"1\",\"2\",\"3\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\"4\",\"5\",\"6\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\"7\",\"8\",\"9\",\".\",\".\",\".\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\"4\",\"5\",\"6\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\"7\",\"8\",\"9\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\"1\",\"2\",\"3\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\"7\",\"8\",\"9\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\"1\",\"2\",\"3\"]\n... ,[\".\",\".\",\".\",\".\",\".\",\".\",\"4\",\"5\",\"6\"]\n... ])\nTrue\n>>> is_valid_sudoku_board([\n...  [\"1\",\"2\",\"3\",\".\",\".\",\".\",\"5\",\"6\",\"4\"]\n... ,[\"4\",\"5\",\"6\",\".\",\".\",\".\",\"8\",\"9\",\"7\"]\n... ,[\"7\",\"8\",\"9\",\".\",\".\",\".\",\"2\",\"3\",\"1\"]\n... ,[\".\",\".\",\".\",\"4\",\"5\",\"6\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\"7\",\"8\",\"9\",\".\",\".\",\".\"]\n... ,[\".\",\".\",\".\",\"1\",\"2\",\"3\",\".\",\".\",\".\"]\n... ,[\"3\",\"1\",\"2\",\".\",\".\",\".\",\"7\",\"8\",\"9\"]\n... ,[\"6\",\"4\",\"5\",\".\",\".\",\".\",\"1\",\"2\",\"3\"]\n... ,[\"9\",\"7\",\"8\",\".\",\".\",\".\",\"4\",\"5\",\"6\"]\n... ])\nTrue\n>>> is_valid_sudoku_board([\n...  [\"1\",\"2\",\"3\",\"4\",\"5\",\"6\",\"7\",\"8\",\"9\"]\n... ,[\"2\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"8\"]\n... ,[\"3\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"7\"]\n... ,[\"4\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"6\"]\n... ,[\"5\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"5\"]\n... ,[\"6\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"4\"]\n... ,[\"7\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"3\"]\n... ,[\"8\",\".\",\".\",\".\",\".\",\".\",\".\",\".\",\"2\"]\n... ,[\"9\",\"8\",\"7\",\"6\",\"5\",\"4\",\"3\",\"2\",\"1\"]\n... ])\nFalse\n>>> is_valid_sudoku_board([\n...  [\"1\",\"2\",\"3\",\"8\",\"9\",\"7\",\"5\",\"6\",\"4\"]\n... ,[\"4\",\"5\",\"6\",\"2\",\"3\",\"1\",\"8\",\"9\",\"7\"]\n... ,[\"7\",\"8\",\"9\",\"5\",\"6\",\"4\",\"2\",\"3\",\"1\"]\n... ,[\"2\",\"3\",\"1\",\"4\",\"5\",\"6\",\"9\",\"7\",\"8\"]\n... ,[\"5\",\"6\",\"4\",\"7\",\"8\",\"9\",\"3\",\"1\",\"2\"]\n... ,[\"8\",\"9\",\"7\",\"1\",\"2\",\"3\",\"6\",\"4\",\"5\"]\n... ,[\"3\",\"1\",\"2\",\"6\",\"4\",\"5\",\"7\",\"8\",\"9\"]\n... ,[\"6\",\"4\",\"5\",\"9\",\"7\",\"8\",\"1\",\"2\",\"3\"]\n... ,[\"9\",\"7\",\"8\",\"3\",\"1\",\"2\",\"4\",\"5\",\"6\"]\n... ])\nTrue\n>>> is_valid_sudoku_board([[\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"]])\nTraceback (most recent call last):\n    ...\nValueError: Sudoku boards must be 9x9 squares.\n>>> is_valid_sudoku_board(\n...        [[\"1\"], [\"2\"], [\"3\"], [\"4\"], [\"5\"], [\"6\"], [\"7\"], [\"8\"], [\"9\"]]\n...  )\nTraceback (most recent call last):\n    ...\nValueError: Sudoku boards must be 9x9 squares."
  [sudoku-board] (let [box (atom nil) box-values (atom nil) col (atom nil) col-values (atom nil) error-message (atom nil) row (atom nil) row-values (atom nil) value (atom nil)] (try (when (py-truthy (let [__b1 (not (py-eq (count sudoku-board) NUM-SQUARES))] (if (py-truthy __b1) __b1 (boolean (some py-truthy (doall (for [row (py-iter sudoku-board)] (not (py-eq (count row) NUM-SQUARES))))))))) (reset! error-message (str "Sudoku boards must be " (py-str NUM-SQUARES) "x" (py-str NUM-SQUARES) " squares.")) (throw (ex-info (py-str @error-message) {:type :ValueError}))) (reset! row-values {}) (reset! col-values {}) (reset! box-values {}) (doseq [__i2 (py-iter (range NUM-SQUARES))] (reset! row __i2) (doseq [__i3 (py-iter (range NUM-SQUARES))] (try (reset! col __i3) (reset! value (py-get (py-get sudoku-board @row) @col)) (when (py-truthy (py-eq @value EMPTY-CELL)) (throw (ex-info "__continue__" {:__control__ true :__continue__ true}))) (reset! box [(py-floordiv @row 3) (py-floordiv @col 3)]) (when (py-truthy (let [__b5 (py-in @value (py-ddget @row-values @row #{}))] (if (py-truthy __b5) __b5 (let [__b4 (py-in @value (py-ddget @col-values @col #{}))] (if (py-truthy __b4) __b4 (py-in @value (py-ddget @box-values @box #{}))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (conj (set (py-ddget @row-values @row #{})) @value) (conj (set (py-ddget @col-values @col #{})) @value) (conj (set (py-ddget @box-values @box #{})) @value) (catch clojure.lang.ExceptionInfo __e (if (:__continue__ (ex-data __e)) nil (throw __e)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(def NUM-SQUARES 9)

(def EMPTY-CELL ".")
