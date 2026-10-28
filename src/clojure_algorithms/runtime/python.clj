(ns clojure-algorithms.runtime.python
  "Shared Python-semantics runtime for machine-transpiled ports.

  These helpers emulate Python operators, truthiness, indexing, dunder
  dispatch, and builtins so transpiled namespaces stay small. They were
  extracted verbatim from the per-file preludes emitted by the porting
  transpiler: every copy was structurally identical, so all transpiled
  namespaces now require this one instead of carrying their own copy.
  Hand-written ports must NOT use these helpers; write idiomatic Clojure."
  {:no-doc true}
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(declare py-rng py-abs py-add py-assoc-in py-assoc-one py-band py-bin py-bxor py-ceil py-cmp-safe py-combinations py-count py-counter py-ddget py-dict py-dict-update py-dissoc-in py-dissoc-one py-div py-endswith py-eq py-factorial py-float py-float-str py-floor py-floordiv py-fmt py-fmt-num py-gauss py-gcd py-ge py-get py-get-in py-gt py-hex py-in py-index py-index-norm py-insert py-int py-iter py-just py-le py-list py-log py-log2 py-lt py-matmul py-max py-max-by py-min py-min-by py-mod py-mul py-neg py-no-dunder py-num-bool py-oct py-permutations py-pop py-pop-rest py-pop2 py-pow py-powmod py-quote py-rand py-randint py-re-compile py-re-findall py-re-flags py-remove-first py-repeat-vec py-repr py-round py-seed py-set py-slice py-slice-assign py-sorted py-split py-startswith py-stat-mean py-stat-stdev py-stat-variance py-str py-strip py-struct-expand py-struct-pack py-struct-tokens py-struct-unpack py-sub py-system py-truthy py-truthy? py-type py-uniform py-update py-zfill)

(def py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn py-abs [x]
  (cond (boolean? x) (if x 1 0)
        (number? x) (if (neg? x) (-' x) x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :abs x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for abs()" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for abs()" {:type :TypeError}))))

(defn py-add [a b]
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

(defn py-assoc-in [c ks v]
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

(defn py-assoc-one [c k v]
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

(defn py-band [a b]
  (cond (and (boolean? a) (boolean? b)) (and a b)
        (and (set? a) (set? b)) (clojure.set/intersection a b)
        (and (integer? (py-num-bool a)) (integer? (py-num-bool b)))
        (.and (.toBigInteger (bigint (py-num-bool a)))
              (.toBigInteger (bigint (py-num-bool b))))
        (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :band a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for &" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :band-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for &" {:type :TypeError})) r))
        :else (throw (ex-info "unsupported operand for &" {:type :TypeError}))))

(defn py-bin [x]
  (if (integer? (py-num-bool x))
    (let [b (biginteger (py-num-bool x))]
      (if (neg? b) (str "-0b" (.toString (.negate b) 2)) (str "0b" (.toString b 2))))
    (throw (ex-info "bin() argument can't be converted" {:type :TypeError}))))

(defn py-bxor [a b]
  (cond (and (boolean? a) (boolean? b)) (not= a b)
        (and (set? a) (set? b))
        (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))
        (and (integer? (py-num-bool a)) (integer? (py-num-bool b)))
        (.xor (.toBigInteger (bigint (py-num-bool a)))
              (.toBigInteger (bigint (py-num-bool b))))
        (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :bxor a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for ^" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :bxor-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for ^" {:type :TypeError})) r))
        :else (throw (ex-info "unsupported operand for ^" {:type :TypeError}))))

(defn py-ceil [x]
  (if (integer? x) x (long (Math/ceil (double x)))))

(defn py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn py-combinations [xs r]
  (let [v (vec xs) n (count v)]
    (cond (neg? r) (throw (ex-info "r must be non-negative" {:type :ValueError}))
          (zero? r) [[]]
          (> r n) []
          :else (vec (mapcat (fn [i]
                               (map #(vec (cons (nth v i) %))
                                    (py-combinations (subvec v (inc i)) (dec r))))
                             (range (- (inc n) r)))))))

(defn py-count [c x]
  (cond (and (string? c) (string? x))
        (if (empty? x) (inc (count c))
            (loop [n 0 i 0]
              (let [j (.indexOf ^String c ^String x (int i))]
                (if (neg? j) n (recur (inc n) (+ j (count x)))))))
        (sequential? c) (count (filter #(py-eq % x) c))
        :else (throw (ex-info "no attribute 'count'" {:type :AttributeError}))))

(defn py-counter [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (string? x) (frequencies (map str (seq x)))
        :else (frequencies (py-iter x))))

(defn py-ddget [c k d]
  (if (map? c) (get c k d) (py-get c k)))

(defn py-dict [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (sequential? x) (into {} (map (fn [p] [(nth p 0) (nth p 1)]) x))
        :else (throw (ex-info "cannot convert to dict" {:type :TypeError}))))

(defn py-dict-update [d x extra]
  (let [d (if (map? d) d (throw (ex-info "update on non-dict" {:type :AttributeError})))]
    (merge d (cond (nil? x) {}
                   (map? x) x
                   (sequential? x) (into {} (map (fn [p] [(nth p 0) (nth p 1)]) x))
                   :else (throw (ex-info "cannot convert to dict" {:type :TypeError})))
           extra)))

(defn py-dissoc-in [c ks]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-dissoc-in (py-get c k) more))
        (let [r (py-dunder :delitem c k)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (dissoc c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-dissoc-in (py-get c k) more))
        (py-dissoc-one c k)))))

(defn py-dissoc-one [c k]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) [])
        (map? c) (if (contains? c k) (dissoc c k)
                     (throw (ex-info (str "key: " k) {:type :KeyError})))
        (string? c) (throw (ex-info "does not support item deletion" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "deletion index out of range" {:type :IndexError})))
                (vec (concat (subvec vv 0 ii) (subvec vv (inc ii)))))))

(defn py-div [a b]
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

(defn py-endswith [s suf]
  (if (sequential? suf)
    (boolean (some #(clojure.string/ends-with? s (str %)) suf))
    (clojure.string/ends-with? s (str suf))))

(defn py-eq [a b]
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

(defn py-factorial [n]
  (let [n (if (and (float? n) (== n (Math/floor (double n)))) (long n) n)]
    (when (boolean? n) (throw (ex-info "factorial() argument" {:type :TypeError})))
    (when (not (integer? n)) (throw (ex-info "factorial() only accepts integral values" {:type :TypeError})))
    (when (neg? n) (throw (ex-info "factorial() not defined for negative values" {:type :ValueError})))
    (loop [i (long n) acc 1N] (if (<= i 1) acc (recur (dec i) (*' acc i))))))

(defn py-float [x]
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

(defn py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn py-floor [x]
  (if (integer? x) x (long (Math/floor (double x)))))

(defn py-floordiv [a b]
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

(defn py-fmt [spec v]
  (if (or (nil? spec) (= spec ""))
    (py-str v)
    (let [spec (str spec)
          m (re-matches #"((.)?([<>=\^]))?([+\- ])?(#)?(0)?(\d+)?(,)?(?:\.(\d+))?([bcdeEfFgGnosxX%])?" spec)]
      (if (nil? m) (py-str v)
          (let [[_ _ fill align sign alt? zero? width comma? prec typ] m
                typ (if typ (first typ) nil)
                prec (if prec (Long/parseLong prec) (if (contains? #{\f \F \e \E} typ) 6 6))
                num? (or (number? v) (boolean? v))
                s (cond (nil? typ) (py-str v)
                        (= typ \s) (py-str v)
                        (= typ \c) (if (and (string? v) (= 1 (count v))) v
                                       (py-fmt-num (if (boolean? v) (if v 1 0) v) typ prec (some? comma?)))
                        (and (= typ \d) (not num?)) (throw (ex-info "format spec" {:type :ValueError}))
                        :else (py-fmt-num (if (boolean? v) (if v 1 0) v) typ prec (some? comma?)))
                s (if (and (= typ \s) prec (re-find #"\.\d+" spec))
                    (subs s 0 (min (count s) (Long/parseLong prec))) s)
                sign-str (if (and num? (number? v) (neg? (double v))) "" (case sign \+ "+" \  " " ""))
                s (if (and num? (contains? #{\d \f \F \e \E \g \G \%} typ) (not (neg? (double v)))) (str sign-str s) s)
                w (if width (Long/parseLong width) 0)
                pad (- w (count s))]
            (if (<= pad 0) s
                (let [al (or align (if num? \> \<))
                      f (cond fill fill (and zero? num? (= al \>)) \0 :else \space)]
                  (case al
                    \< (str s (apply str (repeat pad f)))
                    \> (str (apply str (repeat pad f)) s)
                    \^ (let [l (quot pad 2)] (str (apply str (repeat l f)) s (apply str (repeat (- pad l) f))))
                    \= (if (and (seq s) (contains? #{\+ \- \space} (first s)))
                         (str (first s) (apply str (repeat pad (if (= f \space) \0 f))) (subs s 1))
                         (str (apply str (repeat pad (if (= f \space) \0 f))) s))))))))))

(defn py-fmt-num [v conv prec comma?]
  (let [d (double v)
        s (case conv
            \f (format (str "%." prec "f") d)
            \F (clojure.string/upper-case (format (str "%." prec "f") d))
            \e (format (str "%." prec "e") d)
            \E (clojure.string/upper-case (format (str "%." prec "e") d))
            \g (let [p (max 1 prec)] (format (str "%." p "g") d))
            \G (let [p (max 1 prec)] (clojure.string/upper-case (format (str "%." p "g") d)))
            \d (if (integer? v) (str v) (str (long d)))
            \b (.toString (biginteger (long d)) 2)
            \o (.toString (biginteger (long d)) 8)
            \x (.toString (biginteger (long d)) 16)
            \X (clojure.string/upper-case (.toString (biginteger (long d)) 16))
            \c (str (char (long d)))
            \% (str (* d 100.0) "%")
            (str v))]
    (if comma? (clojure.string/replace s #"(\d)(?=(\d{3})+(?!\d))" "$1,") s)))

(defn py-gauss [mu sigma]
  (let [u1 (max 1e-300 (py-rand)) u2 (py-rand)
        z (* (Math/sqrt (* -2.0 (Math/log u1))) (Math/cos (* 2.0 Math/PI u2)))]
    (+ (double mu) (* (double sigma) z))))

(defn py-gcd [& xs]
  (if (empty? xs) 0
      (reduce (fn [a b]
                (let [a (py-num-bool a) b (py-num-bool b)]
                  (when (or (not (integer? a)) (not (integer? b)))
                    (throw (ex-info "gcd() arguments must be integers" {:type :TypeError})))
                  (loop [x (bigint (if (neg? a) (-' a) a)) y (bigint (if (neg? b) (-' b) b))]
                    (if (zero? y) x (recur y (rem x y))))))
              xs)))

(defn py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn py-get [c k]
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

(defn py-get-in [c ks]
  (if (and (instance? clojure.lang.IRecord c) (> (count ks) 1))
    (let [r (py-dunder :getitem c (vec ks))]
      (if (identical? r py-no-dunder)
        (reduce py-get c ks)
        r))
    (reduce py-get c ks)))

(defn py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn py-hex [x]
  (if (integer? (py-num-bool x))
    (let [b (biginteger (py-num-bool x))]
      (if (neg? b) (str "-0x" (.toString (.negate b) 16)) (str "0x" (.toString b 16))))
    (throw (ex-info "hex() argument can't be converted" {:type :TypeError}))))

(defn py-in [x c]
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

(defn py-index [c x]
  (cond (string? c) (let [i (.indexOf ^String c ^String (str x))]
                      (if (neg? i)
                        (throw (ex-info "substring not found" {:type :ValueError})) i))
        (sequential? c) (let [i (first (keep-indexed (fn [j e] (when (py-eq e x) j)) c))]
                          (if (nil? i)
                            (throw (ex-info "not in list" {:type :ValueError})) i))
        :else (throw (ex-info "no attribute 'index'" {:type :AttributeError}))))

(defn py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn py-insert [c i x]
  (if (or (sequential? c) (nil? c))
    (let [v (vec c) n (count v)
          i (cond (boolean? i) (if i 1 0)
                  (integer? i) (long i)
                  :else (throw (ex-info "indices must be integers" {:type :TypeError})))
          ii (cond (neg? i) (max 0 (+ n i)) (> i n) n :else i)]
      (vec (concat (subvec v 0 ii) [x] (subvec v ii))))
    (throw (ex-info "no attribute 'insert'" {:type :AttributeError}))))

(defn py-int [x base]
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

(defn py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn py-just [side s w fill]
  (let [s (str s) w (long w) f (str (first (str fill)))
        pad (max 0 (- w (count s)))]
    (case side
      :left (str s (apply str (repeat pad f)))
      :right (str (apply str (repeat pad f)) s)
      :center (let [l (quot pad 2) r (- pad l)]
                (str (apply str (repeat l f)) s (apply str (repeat r f)))))))

(defn py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn py-log
  ([x] (if (and (number? (py-num-bool x)) (pos? (double x))) (Math/log (double x))
           (throw (ex-info "math domain error" {:type :ValueError}))))
  ([x b] (let [b (double b)]
           (when (or (neg? b) (zero? b)) (throw (ex-info "math domain error" {:type :ValueError})))
           (when (== b 1.0) (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
           (/ (py-log x) (Math/log b)))))

(defn py-log2 [x]
  (if (and (number? (py-num-bool x)) (pos? (double x))) (/ (Math/log (double x)) (Math/log 2.0))
      (throw (ex-info "math domain error" {:type :ValueError}))))

(defn py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn py-matmul [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :matmul a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for @" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :matmul-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for @" {:type :TypeError})) r))
        (and (sequential? a) (sequential? b))
        (let [row? (fn [x] (and (sequential? x) (every? number? x)))]
          (cond (and (row? a) (row? b)) (reduce +' 0 (map *' a b))
                (and (sequential? (first a)) (row? b))
                (mapv (fn [row] (reduce +' 0 (map *' row b))) a)
                (and (row? a) (sequential? (first b)))
                (mapv (fn [j] (reduce +' 0 (map *' a (map #(nth % j) b)))) (range (count (first b))))
                :else (let [bt (apply mapv vector b)]
                        (mapv (fn [row] (mapv (fn [col] (reduce +' 0 (map *' row col))) bt)) a))))
        :else (throw (ex-info "unsupported operand for @" {:type :TypeError}))))

(defn py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn py-mod [a b]
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

(defn py-mul [a b]
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

(defn py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn py-oct [x]
  (if (integer? (py-num-bool x))
    (let [b (biginteger (py-num-bool x))]
      (if (neg? b) (str "-0o" (.toString (.negate b) 8)) (str "0o" (.toString b 8))))
    (throw (ex-info "oct() argument can't be converted" {:type :TypeError}))))

(defn py-permutations [xs r]
  (let [v (vec xs) n (count v)]
    (cond (neg? r) (throw (ex-info "r must be non-negative" {:type :ValueError}))
          (zero? r) [[]]
          (> r n) []
          :else (vec (mapcat (fn [i]
                               (map #(vec (cons (nth v i) %))
                                    (py-permutations
                                     (vec (concat (subvec v 0 i)
                                                  (subvec v (inc i))))
                                     (dec r))))
                             (range n))))))

(defn py-pop
  ([c k] (py-pop c k :py-no-default))
  ([c k dflt]
   (cond (map? c) (if (contains? c k) (get c k)
                      (if (= dflt :py-no-default)
                        (throw (ex-info (str "key: " k) {:type :KeyError})) dflt))
         (string? c) (throw (ex-info "no attribute 'pop'" {:type :AttributeError}))
         (set? c) (if (nil? k)
                    (if (empty? c)
                      (throw (ex-info "pop from empty set" {:type :KeyError}))
                      (first c))
                    (if (contains? c k) k
                        (throw (ex-info (str "key: " k) {:type :KeyError}))))
         :else (let [v (vec c)]
                 (if (nil? k)
                   (if (empty? v)
                     (throw (ex-info "pop from empty list" {:type :IndexError}))
                     (peek v))
                   (nth v (py-index-norm k (count v))))))))

(defn py-pop-rest
  ([c k] (py-pop-rest c k :py-no-default))
  ([c k dflt] (second (py-pop2 c k dflt))))

(defn py-pop2
  ([c k] (py-pop2 c k :py-no-default))
  ([c k dflt]
   (cond (map? c) (if (contains? c k) [(get c k) (dissoc c k)]
                      (if (= dflt :py-no-default)
                        (throw (ex-info (str "key: " k) {:type :KeyError})) [dflt c]))
         (set? c) (if (nil? k)
                    (if (empty? c)
                      (throw (ex-info "pop from empty set" {:type :KeyError}))
                      [(first c) (disj c (first c))])
                    (if (contains? c k) [k (disj c k)]
                        (throw (ex-info (str "key: " k) {:type :KeyError}))))
         :else (let [v (vec c)]
                 (if (nil? k)
                   (if (empty? v)
                     (throw (ex-info "pop from empty list" {:type :IndexError}))
                     [(peek v) (pop v)])
                   (let [i (py-index-norm k (count v))]
                     [(nth v i) (vec (concat (subvec v 0 i) (subvec v (inc i))))]))))))

(defn py-pow [a b]
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

(defn py-powmod [a b m]
  (let [a (py-num-bool a) b (py-num-bool b) m (py-num-bool m)]
    (when (or (not (integer? a)) (not (integer? b)) (not (integer? m)))
      (throw (ex-info "pow() 3rd argument not allowed unless all arguments are integers" {:type :TypeError})))
    (when (zero? m) (throw (ex-info "pow() 3rd argument cannot be 0" {:type :ValueError})))
    (let [bi (biginteger a) e (long b) mm (biginteger m)]
      (bigint (if (neg? b)
                (.modPow (.modInverse bi mm) (java.math.BigInteger/valueOf (- e)) mm)
                (.modPow bi (java.math.BigInteger/valueOf e) mm))))))

(defn py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn py-rand [] (.nextDouble ^java.util.Random @py-rng))

(defn py-randint [a b]
  (let [a (long a) b (long b) n (inc (- b a))]
    (if (<= n 0) (throw (ex-info "empty range" {:type :ValueError}))
        (if (<= n Integer/MAX_VALUE) (+ a (.nextInt ^java.util.Random @py-rng (int n)))
            (+ a (long (* (py-rand) n)))))))

(defn py-re-compile [pat flags]
  (if (instance? java.util.regex.Pattern pat) pat
      (try (java.util.regex.Pattern/compile (str pat) (py-re-flags flags))
           (catch java.util.regex.PatternSyntaxException e
             (throw (ex-info (str e) {:type :ValueError}))))))

(defn py-re-findall [pat s flags]
  (let [rx (py-re-compile pat flags)
        ^java.util.regex.Matcher m (re-matcher rx (str s))]
    (loop [out []]
      (if (.find m)
        (let [ng (.groupCount m)]
          (recur (conj out (cond (zero? ng) (.group m)
                                 (= ng 1) (.group m 1)
                                 :else (vec (map #(.group m (int %)) (range 1 (inc ng))))))))
        out))))

(defn py-re-flags [f]
  (let [f (long (if (nil? f) 0 f))]
    (bit-or (if (pos? (bit-and f 2)) java.util.regex.Pattern/CASE_INSENSITIVE 0)
            (if (pos? (bit-and f 8)) java.util.regex.Pattern/MULTILINE 0)
            (if (pos? (bit-and f 16)) java.util.regex.Pattern/DOTALL 0)
            (if (pos? (bit-and f 64)) java.util.regex.Pattern/COMMENTS 0))))

(defn py-remove-first [c x]
  (cond (set? c) (if (contains? c x) (disj c x)
                     (throw (ex-info (str "key: " x) {:type :KeyError})))
        (sequential? c) (let [v (vec c)
                              i (first (keep-indexed (fn [j e] (when (py-eq e x) j)) v))]
                          (if (nil? i)
                            (throw (ex-info "list.remove(x): x not in list" {:type :ValueError}))
                            (vec (concat (subvec v 0 i) (subvec v (inc i))))))
        :else (throw (ex-info "no attribute 'remove'" {:type :AttributeError}))))

(defn py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn py-repr [v]
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

(def py-rng (atom (java.util.Random.)))

(defn py-round
  ([x] (long (Math/rint (double x))))
  ([x n]
   (if (nil? n) (py-round x)
       (let [nn (long n)]
         (if (>= nn 0)
           (let [f (Math/pow 10.0 (double nn))]
             (/ (Math/rint (* (double x) f)) f))
           (let [f (Math/pow 10.0 (double (- nn)))]
             (* (Math/rint (/ (double x) f)) f)))))))

(defn py-seed [s]
  (reset! py-rng (if (nil? s) (java.util.Random.)
                     (java.util.Random. (long (if (integer? s) s (hash s))))))
  nil)

(defn py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn py-slice [c lo hi step]
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

(defn py-slice-assign [c lo hi step v]
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

(defn py-sorted [c]
  (cond (string? c) (vec (sort (map str (seq c))))
        (map? c) (vec (sort (keys c)))
        :else (try (vec (sort c))
                   (catch ClassCastException e
                     (throw (ex-info "not orderable" {:type :TypeError}))))))

(defn py-split
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

(defn py-startswith [s pre]
  (if (sequential? pre)
    (boolean (some #(clojure.string/starts-with? s (str %)) pre))
    (clojure.string/starts-with? s (str pre))))

(defn py-stat-mean [c]
  (let [v (vec c)]
    (when (empty? v) (throw (ex-info "mean requires at least one data point" {:type :ValueError})))
    (let [r (/ (reduce +' 0 v) (count v))]
      (if (ratio? r) (double r) r))))

(defn py-stat-stdev [c] (Math/sqrt (double (py-stat-variance c))))

(defn py-stat-variance [c]
  (let [v (vec c)]
    (when (< (count v) 2) (throw (ex-info "variance requires at least two data points" {:type :ValueError})))
    (let [m (/ (reduce +' 0 v) (count v))
          r (/ (reduce +' 0 (map (fn [x] (let [d (-' x m)] (*' d d))) v)) (dec (count v)))]
      (if (ratio? r) (double r) r))))

(defn py-str
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

(defn py-strip [s chars side]
  (if (nil? chars)
    (case side :left (clojure.string/replace s #"^\s+" "") :right (clojure.string/replace s #"\s+$" "") (clojure.string/trim s))
    (let [cs (set (seq chars))
          drop-l (fn [sq] (drop-while cs sq))
          drop-r (fn [sq] (reverse (drop-while cs (reverse sq))))]
      (apply str (case side :left (drop-l (seq s)) :right (drop-r (seq s)) (drop-r (drop-l (seq s))))))))

(defn py-struct-expand [toks]
  (mapcat (fn [[n c]] (if (contains? #{\s \x \p} c) [[n c]] (repeat n [1 c]))) toks))

(defn py-struct-pack [fmt vals]
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

(defn py-struct-tokens [fmt]
  (let [s (str fmt)
        order (if (and (seq s) (contains? #{\< \> \! \=} (first s))) (first s) \@)
        body (if (contains? #{\< \> \! \= \@} (first s)) (subs s 1) s)]
    [order (map (fn [[_ n c]] [(if (empty? n) 1 (Long/parseLong n)) (first c)])
                (re-seq #"(\d*)([xcbB?hHiIlLqQfdspP])" body))]))

(defn py-struct-unpack [fmt b]
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

(defn py-sub [a b]
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

(defn py-system [cmd]
  (try (.waitFor (.start (doto (ProcessBuilder. ^java.util.List ["sh" "-c" (str cmd)]) (.inheritIO))))
       (catch Exception _ 0)))

(defn py-truthy [v]
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

(defn py-truthy?
  "Python-like truthiness for numbers, strings and nil."
  [x]
  (cond
    (nil? x) false
    (false? x) false
    (true? x) true
    (number? x) (not (zero? x))
    (string? x) (boolean (seq x))
    :else true))

(defn py-type [x]
  (cond (nil? x) :none-type
        (boolean? x) :bool
        (integer? x) :int
        (float? x) :float
        (string? x) :str
        (set? x) :set
        (map? x) :dict
        (sequential? x) :list
        :else (class x)))

(defn py-uniform [a b]
  (+ (double a) (* (py-rand) (- (double b) (double a)))))

(defn py-update [c x]
  (cond (map? c) (py-dict-update c x {})
        (set? c) (clojure.set/union c (py-set x))
        :else (throw (ex-info "no attribute 'update'" {:type :AttributeError}))))

(defn py-zfill [s w]
  (let [s (str s) w (long w)]
    (if (>= (count s) w) s
        (if (and (seq s) (contains? #{\+ \-} (first s)))
          (str (first s) (apply str (repeat (- w (count s)) \0)) (subs s 1))
          (str (apply str (repeat (- w (count s)) \0)) s)))))

