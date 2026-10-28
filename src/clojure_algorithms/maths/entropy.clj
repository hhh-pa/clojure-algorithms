(ns clojure-algorithms.maths.entropy
  "Implementation of entropy of information\nhttps://en.wikipedia.org/wiki/Entropy_(information_theory)"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare calculate-prob analyze-text main)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-slice py-add py-sub py-repeat-vec py-mul py-div py-neg py-list py-counter py-float-str py-quote py-repr py-str py-int py-round py-fmt-num py-fmt py-log2)

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

(defn- py-get-in [c ks]
  (if (and (instance? clojure.lang.IRecord c) (> (count ks) 1))
    (let [r (py-dunder :getitem c (vec ks))]
      (if (identical? r py-no-dunder)
        (reduce py-get c ks)
        r))
    (reduce py-get c ks)))

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

(defn- py-counter [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (string? x) (frequencies (map str (seq x)))
        :else (frequencies (py-iter x))))

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

(defn- py-round
  ([x] (long (Math/rint (double x))))
  ([x n]
   (if (nil? n) (py-round x)
       (let [nn (long n)]
         (if (>= nn 0)
           (let [f (Math/pow 10.0 (double nn))]
             (/ (Math/rint (* (double x) f)) f))
           (let [f (Math/pow 10.0 (double (- nn)))]
             (* (Math/rint (/ (double x) f)) f)))))))

(defn- py-fmt-num [v conv prec comma?]
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

(defn- py-fmt [spec v]
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

(defn- py-log2 [x]
  (if (and (number? (py-num-bool x)) (pos? (double x))) (/ (Math/log (double x)) (Math/log 2.0))
      (throw (ex-info "math domain error" {:type :ValueError}))))

(defn calculate-prob "This method takes path and two dict as argument\nand than calculates entropy of them.\n:param dict:\n:param dict:\n:return: Prints\n1) Entropy of information based on 1 alphabet\n2) Entropy of information based on couples of 2 alphabet\n3) print Entropy of H(X n|Xn-1)\n\nText from random books. Also, random quotes.\n>>> text = (\"Behind Winston's back the voice \"\n...         \"from the telescreen was still \"\n...         \"babbling and the overfulfilment\")\n>>> calculate_prob(text)\n4.0\n6.0\n2.0\n\n>>> text = (\"The Ministry of Truth—Minitrue, in Newspeak [Newspeak was the official\"\n...         \"face in elegant lettering, the three\")\n>>> calculate_prob(text)\n4.0\n5.0\n1.0\n>>> text = (\"Had repulsive dashwoods suspicion sincerity but advantage now him. \"\n...         \"Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. \"\n...         \"You greatest jointure saw horrible. He private he on be imagine \"\n...         \"suppose. Fertile beloved evident through no service elderly is. Blind \"\n...         \"there if every no so at. Own neglected you preferred way sincerity \"\n...         \"delivered his attempted. To of message cottage windows do besides \"\n...         \"against uncivil.  Delightful unreserved impossible few estimating \"\n...         \"men favourable see entreaties. She propriety immediate was improving. \"\n...         \"He or entrance humoured likewise moderate. Much nor game son say \"\n...         \"feel. Fat make met can must form into gate. Me we offending prevailed \"\n...         \"discovery.\")\n>>> calculate_prob(text)\n4.0\n7.0\n3.0"
  [text] (let [all-sum (atom nil) ch (atom nil) ch0 (atom nil) ch1 (atom nil) my-alphas (atom nil) my-fir-sum (atom nil) my-sec-sum (atom nil) my-str (atom nil) prob (atom nil) sequence (atom nil) single-char-strings (atom nil) two-char-strings (atom nil)] (let [__d1 (vec (analyze-text text))] (reset! single-char-strings (nth __d1 0)) (reset! two-char-strings (nth __d1 1))) (reset! my-alphas (py-list (py-add " " "abcdefghijklmnopqrstuvwxyz"))) (reset! all-sum (reduce +' 0 (vec (vals @single-char-strings)))) (reset! my-fir-sum 0) (doseq [__i2 (py-iter @my-alphas)] (reset! ch __i2) (when (py-truthy (py-in @ch @single-char-strings)) (reset! my-str (py-get @single-char-strings @ch)) (reset! prob (py-div @my-str @all-sum)) (swap! my-fir-sum py-add (py-mul @prob (py-log2 @prob))))) (println (clojure.string/join " " [(py-str (py-fmt ".1f" (py-round (py-mul (py-neg 1) @my-fir-sum))))])) (reset! all-sum (reduce +' 0 (vec (vals @two-char-strings)))) (reset! my-sec-sum 0) (doseq [__i3 (py-iter @my-alphas)] (reset! ch0 __i3) (doseq [__i4 (py-iter @my-alphas)] (reset! ch1 __i4) (reset! sequence (py-add @ch0 @ch1)) (when (py-truthy (py-in @sequence @two-char-strings)) (reset! my-str (py-get @two-char-strings @sequence)) (reset! prob (py-div (py-int @my-str nil) @all-sum)) (swap! my-sec-sum py-add (py-mul @prob (py-log2 @prob)))))) (println (clojure.string/join " " [(py-str (py-fmt ".1f" (py-round (py-mul (py-neg 1) @my-sec-sum))))])) (println (clojure.string/join " " [(py-str (py-fmt ".1f" (py-round (py-sub (py-mul (py-neg 1) @my-sec-sum) (py-mul (py-neg 1) @my-fir-sum)))))])) nil))

(defn analyze-text "Convert text input into two dicts of counts.\nThe first dictionary stores the frequency of single character strings.\nThe second dictionary stores the frequency of two character strings."
  [text] (let [i (atom nil) single-char-strings (atom nil) two-char-strings (atom nil)] (try (reset! single-char-strings (py-counter [])) (reset! two-char-strings (py-counter [])) (swap! single-char-strings (fn [__c] (py-assoc-in __c [(py-get text (py-neg 1))] (py-add (get __c (py-get text (py-neg 1)) 0) 1)))) (swap! two-char-strings (fn [__c] (py-assoc-in __c [(py-add " " (py-get text 0))] (py-add (get __c (py-add " " (py-get text 0)) 0) 1)))) (doseq [__i1 (py-iter (range (py-sub (count text) 1)))] (reset! i __i1) (swap! single-char-strings (fn [__c] (py-assoc-in __c [(py-get text @i)] (py-add (get __c (py-get text @i) 0) 1)))) (swap! two-char-strings (fn [__c] (py-assoc-in __c [(py-slice text @i (py-add @i 2) nil)] (py-add (get __c (py-slice text @i (py-add @i 2) nil) 0) 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@single-char-strings @two-char-strings]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [& _args]
  ;; stub: original requires method .testmod; pure-Clojure port not applicable
  (throw (ex-info "requires method .testmod" {:type :UnsupportedOperation})))

;; NOTE: main stubbed (method .testmod)
