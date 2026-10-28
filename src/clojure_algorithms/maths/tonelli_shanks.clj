(ns clojure-algorithms.maths.tonelli-shanks
  "Tonelli-Shanks algorithm for modular square roots.\n\nGiven an odd prime modulus ``prime`` and an integer ``residue``, find an\ninteger ``root`` such that ``root ** 2 ≡ residue (mod prime)``, or report that\nno square root exists.\n\nThe algorithm is efficient when ``prime ≡ 3 (mod 4)`` (a single\nexponentiation) and uses the full Tonelli-Shanks procedure for\n``prime ≡ 1 (mod 4)``.\n\nhttps://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare legendre-symbol tonelli-shanks)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-repeat-vec py-mul py-floordiv py-mod py-pow py-powmod py-neg py-min py-min-by py-max py-max-by py-float-str py-quote py-repr py-str)

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

(defn legendre-symbol "Compute the Legendre symbol (residue / prime).\n\nReturns 1 if residue is a quadratic residue modulo prime (and residue\nis not divisible by prime), -1 if it is a non-residue, and 0 if\nresidue ≡ 0 (mod prime).\n\n>>> legendre_symbol(2, 7)\n1\n>>> legendre_symbol(3, 7)\n-1\n>>> legendre_symbol(14, 7)\n0\n>>> legendre_symbol(5, 11)\n1"
  [residue prime] (let [symbol-v (atom nil)] (try (when (py-truthy (let [__b1 (py-le prime 2)] (if (py-truthy __b1) __b1 (py-eq (py-mod prime 2) 0)))) (throw (ex-info (py-str "prime must be an odd prime") {:type :ValueError}))) (reset! symbol-v (py-powmod (py-mod residue prime) (py-floordiv (py-sub prime 1) 2) prime)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-eq @symbol-v (py-sub prime 1))) (py-neg 1) @symbol-v)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn tonelli-shanks "Return a modular square root of ``residue`` modulo odd prime ``prime``.\n\nIf both roots exist, the smaller non-negative representative is returned.\nRaises ValueError when ``residue`` is not a quadratic residue, or when\n``prime`` is not a valid odd prime modulus for this routine.\n\n>>> tonelli_shanks(5, 41)\n13\n>>> pow(13, 2, 41)\n5\n>>> tonelli_shanks(2, 7)\n3\n>>> pow(3, 2, 7)\n2\n>>> tonelli_shanks(10, 13)\n6\n>>> tonelli_shanks(0, 11)\n0\n>>> tonelli_shanks(8, 17)\n5\n>>> pow(5, 2, 17)\n8\n>>> tonelli_shanks(3, 7)\nTraceback (most recent call last):\n    ...\nValueError: 3 is not a quadratic residue modulo 7\n>>> tonelli_shanks(5, 4)\nTraceback (most recent call last):\n    ...\nValueError: prime must be an odd prime\n>>> tonelli_shanks(5, 1)\nTraceback (most recent call last):\n    ...\nValueError: prime must be an odd prime"
  [residue prime] (let [residue (atom residue)] (let [candidate-i (atom nil) exponent-q (atom nil) least-i (atom nil) message (atom nil) modular-b (atom nil) modular-c (atom nil) modular-r (atom nil) modular-t (atom nil) non-residue (atom nil) power-of-two-s (atom nil) remaining-s (atom nil) root (atom nil) symbol-v (atom nil) test-power (atom nil)] (try (when (py-truthy (let [__b1 (py-le prime 2)] (if (py-truthy __b1) __b1 (py-eq (py-mod prime 2) 0)))) (throw (ex-info (py-str "prime must be an odd prime") {:type :ValueError}))) (swap! residue py-mod prime) (when (py-truthy (py-eq @residue 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (reset! symbol-v (legendre-symbol @residue prime)) (when (py-truthy (not (py-eq @symbol-v 1))) (reset! message (str (py-str @residue) " is not a quadratic residue modulo " (py-str prime))) (throw (ex-info (py-str @message) {:type :ValueError}))) (when (py-truthy (py-eq (py-mod prime 4) 3)) (reset! root (py-powmod @residue (py-floordiv (py-add prime 1) 4) prime)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-min [@root (py-sub prime @root)])}))) (reset! exponent-q (py-sub prime 1)) (reset! power-of-two-s 0) (while (py-truthy (py-eq (py-mod @exponent-q 2) 0)) (swap! exponent-q py-floordiv 2) (swap! power-of-two-s py-add 1)) (reset! non-residue 2) (while (py-truthy (not (py-eq (legendre-symbol @non-residue prime) (py-neg 1)))) (swap! non-residue py-add 1)) (reset! modular-c (py-powmod @non-residue @exponent-q prime)) (reset! modular-r (py-powmod @residue (py-floordiv (py-add @exponent-q 1) 2) prime)) (reset! modular-t (py-powmod @residue @exponent-q prime)) (reset! remaining-s @power-of-two-s) (while (py-truthy (not (py-eq @modular-t 1))) (reset! test-power @modular-t) (reset! least-i 0) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 1 @remaining-s))] (reset! candidate-i __i2) (reset! test-power (py-powmod @test-power 2 prime)) (when (py-truthy (py-eq @test-power 1)) (reset! least-i @candidate-i) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (reset! message (str (py-str @residue) " is not a quadratic residue modulo " (py-str prime))) (throw (ex-info (py-str @message) {:type :ValueError})))) (reset! modular-b (py-powmod @modular-c (.shiftLeft (.toBigInteger (bigint 1)) (int (py-sub (py-sub @remaining-s @least-i) 1))) prime)) (reset! modular-r (py-mod (py-mul @modular-r @modular-b) prime)) (reset! modular-c (py-powmod @modular-b 2 prime)) (reset! modular-t (py-mod (py-mul @modular-t @modular-c) prime)) (reset! remaining-s @least-i)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-min [@modular-r (py-sub prime @modular-r)])})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
