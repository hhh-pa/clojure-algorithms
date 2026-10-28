(ns clojure-algorithms.maths.modular-division
  "Port of maths/modular_division.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare modular-division invert-modulo modular-division2 extended-gcd extended-euclid greatest-common-divisor)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-add py-sub py-repeat-vec py-mul py-floordiv py-mod py-float-str py-quote py-repr py-str)

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

(defn modular-division "Modular Division :\nAn efficient algorithm for dividing b by a modulo n.\n\nGCD ( Greatest Common Divisor ) or HCF ( Highest Common Factor )\n\nGiven three integers a, b, and n, such that gcd(a,n)=1 and n>1, the algorithm should\nreturn an integer x such that 0≤x≤n-1, and  b/a=x(modn) (that is, b=ax(modn)).\n\nTheorem:\na has a multiplicative inverse modulo n iff gcd(a,n) = 1\n\n\nThis find x = b*a^(-1) mod n\nUses ExtendedEuclid to find the inverse of a\n\n>>> modular_division(4,8,5)\n2\n\n>>> modular_division(3,8,5)\n1\n\n>>> modular_division(4, 11, 5)\n4"
  [a b n] (let [-d (atom nil) -t (atom nil) s (atom nil) x (atom nil)] (try (when (py-truthy (py-le n 1)) (throw (ex-info (py-str "Modulus n must be greater than 1") {:type :ValueError}))) (when (py-truthy (py-le a 0)) (throw (ex-info (py-str "Divisor a must be a positive integer") {:type :ValueError}))) (when (py-truthy (not (py-eq (greatest-common-divisor a n) 1))) (throw (ex-info (py-str "a and n must be coprime (gcd(a, n) = 1)") {:type :ValueError}))) (let [__d1 (vec (extended-gcd n a))] (reset! -d (nth __d1 0)) (reset! -t (nth __d1 1)) (reset! s (nth __d1 2))) (reset! x (py-mod (py-mul b @s) n)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @x})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn invert-modulo "This function find the inverses of a i.e., a^(-1)\n\n>>> invert_modulo(2, 5)\n3\n\n>>> invert_modulo(8,7)\n1"
  [a n] (let [-x (atom nil) b (atom nil)] (try (let [__d1 (vec (extended-euclid a n))] (reset! b (nth __d1 0)) (reset! -x (nth __d1 1))) (when (py-truthy (py-lt @b 0)) (reset! b (py-mod (py-add (py-mod @b n) n) n))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @b})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn modular-division2 "This function used the above inversion of a to find x = (b*a^(-1))mod n\n\n>>> modular_division2(4,8,5)\n2\n\n>>> modular_division2(3,8,5)\n1\n\n>>> modular_division2(4, 11, 5)\n4"
  [a b n] (let [s (atom nil) x (atom nil)] (try (reset! s (invert-modulo a n)) (reset! x (py-mod (py-mul b @s) n)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @x})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn extended-gcd "Extended Euclid's Algorithm : If d divides a and b and d = a*x + b*y for integers x\nand y, then d = gcd(a,b)\n>>> extended_gcd(10, 6)\n(2, -1, 2)\n\n>>> extended_gcd(7, 5)\n(1, -2, 3)\n\n** extended_gcd function is used when d = gcd(a,b) is required in output"
  [a b] (let [d (atom nil) p (atom nil) q (atom nil) x (atom nil) y (atom nil)] (try (assert (py-truthy (py-ge a 0))) (assert (py-truthy (py-ge b 0))) (if (py-truthy (py-eq b 0)) (do (let [__d1 (vec [a 1 0])] (reset! d (nth __d1 0)) (reset! x (nth __d1 1)) (reset! y (nth __d1 2)))) (do (let [__d2 (vec (extended-gcd b (py-mod a b)))] (reset! d (nth __d2 0)) (reset! p (nth __d2 1)) (reset! q (nth __d2 2))) (reset! x @q) (reset! y (py-sub @p (py-mul @q (py-floordiv a b)))))) (assert (py-truthy (py-eq (py-mod a @d) 0))) (assert (py-truthy (py-eq (py-mod b @d) 0))) (assert (py-truthy (py-eq @d (py-add (py-mul a @x) (py-mul b @y))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@d @x @y]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn extended-euclid "Extended Euclid\n>>> extended_euclid(10, 6)\n(-1, 2)\n\n>>> extended_euclid(7, 5)\n(-2, 3)"
  [a b] (let [k (atom nil) x (atom nil) y (atom nil)] (try (when (py-truthy (py-eq b 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [1 0]}))) (let [__d1 (vec (extended-euclid b (py-mod a b)))] (reset! x (nth __d1 0)) (reset! y (nth __d1 1))) (reset! k (py-floordiv a b)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@y (py-sub @x (py-mul @k @y))]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn greatest-common-divisor "Euclid's Lemma :  d divides a and b, if and only if d divides a-b and b\nEuclid's Algorithm\n\n>>> greatest_common_divisor(7,5)\n1\n\nNote : In number theory, two integers a and b are said to be relatively prime,\n    mutually prime, or co-prime if the only positive integer (factor) that divides\n    both of them is 1  i.e., gcd(a,b) = 1.\n\n>>> greatest_common_divisor(121, 11)\n11"
  [a b] (let [a (atom a) b (atom b)] (try (when (py-truthy (py-lt @a @b)) (let [__d1 (vec [@b @a])] (reset! a (nth __d1 0)) (reset! b (nth __d1 1)))) (while (py-truthy (not (py-eq (py-mod @a @b) 0))) (let [__d2 (vec [@b (py-mod @a @b)])] (reset! a (nth __d2 0)) (reset! b (nth __d2 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @b})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
