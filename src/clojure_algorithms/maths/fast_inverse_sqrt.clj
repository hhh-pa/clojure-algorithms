(ns clojure-algorithms.maths.fast-inverse-sqrt
  "Fast inverse square root (1/sqrt(x)) using the Quake III algorithm.\nReference: https://en.wikipedia.org/wiki/Fast_inverse_square_root\nAccuracy: https://en.wikipedia.org/wiki/Fast_inverse_square_root#Accuracy"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare fast-inverse-sqrt)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-index-norm py-get py-sub py-repeat-vec py-mul py-float-str py-quote py-repr py-str py-struct-tokens py-struct-expand py-struct-pack py-struct-unpack)

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

(defn- py-struct-tokens [fmt]
  (let [s (str fmt)
        order (if (and (seq s) (contains? #{\< \> \! \=} (first s))) (first s) \@)
        body (if (contains? #{\< \> \! \= \@} (first s)) (subs s 1) s)]
    [order (map (fn [[_ n c]] [(if (empty? n) 1 (Long/parseLong n)) (first c)])
                (re-seq #"(\d*)([xcbB?hHiIlLqQfdspP])" body))]))

(defn- py-struct-expand [toks]
  (mapcat (fn [[n c]] (if (contains? #{\s \x \p} c) [[n c]] (repeat n [1 c]))) toks))

(defn- py-struct-pack [fmt vals]
  (let [[order toks] (py-struct-tokens fmt)
        le? (= order \<)
        bb (java.nio.ByteBuffer/allocate 8192)]
    (.order bb (if le? java.nio.ByteOrder/LITTLE_ENDIAN java.nio.ByteOrder/BIG_ENDIAN))
    (loop [ts (py-struct-expand toks) vs (seq vals)]
      (when (seq ts)
        (let [[[n c] & more] ts]
          (case c
            \x (do (dotimes [_ n] (.put bb (byte 0))) (recur more vs))
            \c (do (.put bb (byte (int (first (str (first vs)))))) (recur more (rest vs)))
            \b (do (.put bb (byte (long (first vs)))) (recur more (rest vs)))
            \B (do (.put bb (unchecked-byte (long (first vs)))) (recur more (rest vs)))
            \? (do (.put bb (byte (if (py-truthy (first vs)) 1 0))) (recur more (rest vs)))
            \h (do (.putShort bb (short (long (first vs)))) (recur more (rest vs)))
            \H (do (.putShort bb (unchecked-short (long (first vs)))) (recur more (rest vs)))
            \i (do (.putInt bb (unchecked-int (long (first vs)))) (recur more (rest vs)))
            \I (do (.putInt bb (unchecked-int (long (first vs)))) (recur more (rest vs)))
            \l (do (.putInt bb (unchecked-int (long (first vs)))) (recur more (rest vs)))
            \L (do (.putInt bb (unchecked-int (long (first vs)))) (recur more (rest vs)))
            \q (do (.putLong bb (long (first vs))) (recur more (rest vs)))
            \Q (do (.putLong bb (unchecked-long (bigint (first vs)))) (recur more (rest vs)))
            \f (do (.putFloat bb (float (double (first vs)))) (recur more (rest vs)))
            \d (do (.putDouble bb (double (first vs))) (recur more (rest vs)))
            \s (let [bs (.getBytes ^String (str (first vs)) "ISO-8859-1")]
                 (dotimes [i n] (.put bb (byte (if (< i (alength ^bytes bs)) (aget ^bytes bs i) 0))))
                 (recur more (rest vs)))
            (recur more vs)))))
    (let [pos (.position bb) arr (.array bb)]
      (vec (map #(bit-and 0xFF (int %)) (take pos arr))))))

(defn- py-struct-unpack [fmt b]
  (let [[order toks] (py-struct-tokens fmt)
        le? (= order \<)
        data (if (string? b) (vec (.getBytes ^String b "ISO-8859-1")) (vec b))
        bb (java.nio.ByteBuffer/wrap (byte-array (map unchecked-byte data)))]
    (.order bb (if le? java.nio.ByteOrder/LITTLE_ENDIAN java.nio.ByteOrder/BIG_ENDIAN))
    (loop [ts (py-struct-expand toks) out []]
      (if (empty? ts) out
          (let [[[n c] & more] ts]
            (case c
              \x (do (.position bb (+ (.position bb) n)) (recur more out))
              \c (recur more (conj out (str (char (bit-and 0xFF (.get bb))))))
              \b (recur more (conj out (long (.get bb))))
              \B (recur more (conj out (long (bit-and 0xFF (.get bb)))))
              \? (recur more (conj out (not (zero? (.get bb)))))
              \h (recur more (conj out (long (.getShort bb))))
              \H (recur more (conj out (long (bit-and 0xFFFF (.getShort bb)))))
              \i (recur more (conj out (long (.getInt bb))))
              \I (recur more (conj out (bigint (bit-and 0xFFFFFFFF (long (.getInt bb))))))
              \l (recur more (conj out (long (.getInt bb))))
              \L (recur more (conj out (bigint (bit-and 0xFFFFFFFF (long (.getInt bb))))))
              \q (recur more (conj out (long (.getLong bb))))
              \Q (let [bs (byte-array 8)] (.get bb ^bytes bs 0 8)
                      (recur more (conj out (bigint (java.math.BigInteger. 1 bs)))))
              \f (recur more (conj out (double (.getFloat bb))))
              \d (recur more (conj out (.getDouble bb)))
              \s (let [bs (byte-array n)] (.get bb ^bytes bs 0 n)
                      (recur more (conj out (String. ^bytes bs "ISO-8859-1"))))
              (recur more out)))))))

(defn fast-inverse-sqrt "Compute the fast inverse square root of a floating-point number using the famous\nQuake III algorithm.\n\n:param float number: Input number for which to calculate the inverse square root.\n:return float: The fast inverse square root of the input number.\n\nExample:\n>>> fast_inverse_sqrt(10)\n0.3156857923527257\n>>> fast_inverse_sqrt(4)\n0.49915357479239103\n>>> fast_inverse_sqrt(4.1)\n0.4932849504615651\n>>> fast_inverse_sqrt(0)\nTraceback (most recent call last):\n    ...\nValueError: Input must be a positive number.\n>>> fast_inverse_sqrt(-1)\nTraceback (most recent call last):\n    ...\nValueError: Input must be a positive number.\n>>> from math import isclose, sqrt\n>>> all(isclose(fast_inverse_sqrt(i), 1 / sqrt(i), rel_tol=0.00132)\n...     for i in range(50, 60))\nTrue"
  [number] (let [i (atom nil) y (atom nil)] (try (when (py-truthy (py-le number 0)) (throw (ex-info (py-str "Input must be a positive number.") {:type :ValueError}))) (reset! i (py-get (py-struct-unpack ">i" (py-struct-pack ">f" [number])) 0)) (reset! i (py-sub 1597463007 (.shiftRight (.toBigInteger (bigint @i)) (int 1)))) (reset! y (py-get (py-struct-unpack ">f" (py-struct-pack ">i" [@i])) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul @y (py-sub 1.5 (py-mul (py-mul (py-mul 0.5 number) @y) @y)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
