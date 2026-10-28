(ns clojure-algorithms.maths.radix2-fft
  "Fast Polynomial Multiplication using radix-2 fast Fourier Transform."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare make-FFT FFT-dft FFT-multiply FFT-str)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice py-add py-sub py-repeat-vec py-mul py-floordiv py-pow py-float-str py-quote py-repr py-str)

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

(defrecord FFT [polyA polyB len-A len-B c-max-length root product])

(defn make-FFT "Constructor for FFT. Fast Polynomial Multiplication using radix-2 fast Fourier Transform.\n\nReference:\nhttps://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#The_radix-2_DIT_case\n\nFor polynomials of degree m and n the algorithms has complexity\nO(n*logn + m*logm)\n\nThe main part of the algorithm is split in two parts:\n    1) __DFT: We compute the discrete fourier transform (DFT) of A and B using a\n    bottom-up dynamic approach -\n    2) __multiply: Once we obtain the DFT of A*B, we can similarly\n    invert it to obtain A*B\n\nThe class FFT takes two polynomials A and B with complex coefficients as arguments;\nThe two polynomials should be represented as a sequence of coefficients starting\nfrom the free term. Thus, for instance x + 2*x^3 could be represented as\n[0,1,0,2] or (0,1,0,2). The constructor adds some zeros at the end so that the\npolynomials have the same length which is a power of 2 at least the length of\ntheir product.\n\nExample:\n\nCreate two polynomials as sequences\n>>> A = [0, 1, 0, 2]  # x+2x^3\n>>> B = (2, 3, 4, 0)  # 2+3x+4x^2\n\nCreate an FFT object with them\n>>> x = FFT(A, B)\n\nPrint product\n>>> x.product  # 2x + 3x^2 + 8x^3 + 6x^4 + 8x^5\n[(-0-0j), (2+0j), (3-0j), (8-0j), (6+0j), (8+0j)]\n\n__str__ test\n>>> print(x)\nA = 0*x^0 + 1*x^1 + 0*x^2 + 2*x^3\nB = 2*x^0 + 3*x^1 + 4*x^2\nA*B = (-0-0j)*x^0 + (2+0j)*x^1 + (3-0j)*x^2 + (8-0j)*x^3 + (6+0j)*x^4 + (8+0j)*x^5"
  [& _args]
  ;; stub: original requires complex(),mpmath,np; pure-Clojure port not applicable
  (throw (ex-info "requires complex(),mpmath,np" {:type :UnsupportedOperation})))

(defn FFT-dft
  [self which] (let [current-root (atom nil) dft (atom nil) i (atom nil) j (atom nil) new-dft (atom nil) next-ncol (atom nil) root (atom nil)] (try (reset! dft (if (py-truthy (py-eq which "A")) (vec (for [x (py-iter (:polyA @self))] [x])) (vec (for [x (py-iter (:polyB @self))] [x])))) (when (py-truthy (py-le (count @dft) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @dft 0)}))) (reset! next-ncol (py-floordiv (:c-max-length @self) 2)) (while (py-truthy (py-gt @next-ncol 0)) (reset! new-dft (vec (for [i (py-iter (range @next-ncol))] []))) (reset! root (py-pow (:root @self) @next-ncol)) (reset! current-root 1) (doseq [__i1 (py-iter (range (py-floordiv (:c-max-length @self) (py-mul @next-ncol 2))))] (reset! j __i1) (doseq [__i2 (py-iter (range @next-ncol))] (reset! i __i2) (conj (vec (py-get @new-dft @i)) (py-add (py-get (py-get @dft @i) @j) (py-mul @current-root (py-get (py-get @dft (py-add @i @next-ncol)) @j))))) (swap! current-root py-mul @root)) (reset! current-root 1) (doseq [__i3 (py-iter (range (py-floordiv (:c-max-length @self) (py-mul @next-ncol 2))))] (reset! j __i3) (doseq [__i4 (py-iter (range @next-ncol))] (reset! i __i4) (conj (vec (py-get @new-dft @i)) (py-sub (py-get (py-get @dft @i) @j) (py-mul @current-root (py-get (py-get @dft (py-add @i @next-ncol)) @j))))) (swap! current-root py-mul @root)) (reset! dft @new-dft) (reset! next-ncol (py-floordiv @next-ncol 2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @dft 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn FFT-multiply
  [& _args]
  ;; stub: original requires complex(),complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex(),complex-attr" {:type :UnsupportedOperation})))

(defn FFT-str
  [self] (let [a (atom nil) b (atom nil) c (atom nil)] (try (reset! a (py-add "A = " (clojure.string/join " + " (map py-str (doall (for [[i coef] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-slice (:polyA @self) nil (:len-A @self) nil)))] (str (py-str coef) "*x^" (py-str i)))))))) (reset! b (py-add "B = " (clojure.string/join " + " (map py-str (doall (for [[i coef] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-slice (:polyB @self) nil (:len-B @self) nil)))] (str (py-str coef) "*x^" (py-str i)))))))) (reset! c (py-add "A*B = " (clojure.string/join " + " (map py-str (doall (for [[i coef] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (:product @self)))] (str (py-str coef) "*x^" (py-str i)))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (str (py-str @a) "\n" (py-str @b) "\n" (py-str @c))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defmethod print-method FFT [o w] (.write w (FFT-str (atom o))))

;; NOTE: FFT __init__ stubbed (complex(),mpmath,np)
;; NOTE: FFT.__multiply stubbed (complex(),complex-attr)
