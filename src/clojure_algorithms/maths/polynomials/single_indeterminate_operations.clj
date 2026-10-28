(ns clojure-algorithms.maths.polynomials.single-indeterminate-operations
  "This module implements a single indeterminate polynomials class
with some basic operations

Reference: https://en.wikipedia.org/wiki/Polynomial")

(defrecord Polynomial [coefficients degree])

(defn make-Polynomial
  "Constructor for Polynomial.

  Coefficients are in order of degree, from smallest to largest.
  Throws ex-info with :type :ValueError when the number of coefficients is
  not degree + 1."
  [degree coefficients]
  (when-not (= (count coefficients) (inc degree))
    (throw (ex-info "The number of coefficients should be equal to the degree + 1."
                    {:type :ValueError})))
  (->Polynomial (vec coefficients) degree))

(defn- pad-to
  [coefficients length]
  (vec (take length (concat coefficients (repeat 0)))))

(defn Polynomial-add
  "Add two polynomials."
  [self polynomial-2]
  (let [degree (max (:degree self) (:degree polynomial-2))]
    (make-Polynomial degree
                     (mapv + (pad-to (:coefficients self) (inc degree))
                           (pad-to (:coefficients polynomial-2) (inc degree))))))

(defn Polynomial-mul
  "Multiply two polynomials."
  [self polynomial-2]
  (let [c1 (:coefficients self)
        c2 (:coefficients polynomial-2)
        d1 (:degree self)
        d2 (:degree polynomial-2)]
    (make-Polynomial (+ d1 d2)
                     (vec (for [k (range (inc (+ d1 d2)))]
                            (reduce + (for [i (range (inc d1))
                                            :let [j (- k i)]
                                            :when (and (<= 0 j) (<= j d2))]
                                        (* (nth c1 i) (nth c2 j)))))))))

(defn Polynomial-sub
  "Subtract polynomial-2 from self."
  [self polynomial-2]
  (Polynomial-add self (Polynomial-mul polynomial-2 (make-Polynomial 0 [-1]))))

(defn Polynomial-neg
  "Negate a polynomial."
  [self]
  (make-Polynomial (:degree self) (mapv - (:coefficients self))))

(defn- integer-pow
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn Polynomial-evaluate
  "Evaluate the polynomial at substitution."
  [self substitution]
  (reduce + 0 (map-indexed (fn [i c] (* c (integer-pow substitution i)))
                           (:coefficients self))))

(defn Polynomial-str
  "Render the polynomial as a string, highest degree first."
  [self]
  (let [{:keys [degree coefficients]} self]
    (reduce (fn [s i]
              (let [c (nth coefficients i)]
                (if (zero? c)
                  s
                  (str s
                       (if (pos? c)
                         (if (empty? s) "" " + ")
                         " - ")
                       (let [a (if (neg? c) (- c) c)]
                         (cond (zero? i) (str a)
                               (= i 1) (str a "x")
                               :else (str a "x^" i)))))))
            ""
            (range degree -1 -1))))

(defn Polynomial-repr
  "Render the polynomial as a string."
  [self]
  (Polynomial-str self))

(defn Polynomial-derivative
  "Return the derivative of the polynomial."
  [self]
  (let [degree (:degree self)]
    (make-Polynomial (dec degree)
                     (mapv * (subvec (:coefficients self) 1) (range 1 (inc degree))))))

(defn Polynomial-integral
  "Return the integral of the polynomial with the given constant (default 0)."
  ([self] (Polynomial-integral self 0))
  ([self constant]
   (make-Polynomial (inc (:degree self))
                    (vec (cons constant
                               (map-indexed (fn [i c] (/ (double c) (inc i)))
                                            (:coefficients self)))))))

(defn Polynomial-eq
  "Check if two polynomials are equal."
  [self polynomial-2]
  (boolean (and (instance? Polynomial polynomial-2)
                (= (:degree self) (:degree polynomial-2))
                (= (:coefficients self) (:coefficients polynomial-2)))))

(defn Polynomial-ne
  "Check if two polynomials are not equal."
  [self polynomial-2]
  (not (Polynomial-eq self polynomial-2)))

(defmethod print-method Polynomial [p w]
  (.write ^java.io.Writer w ^String (Polynomial-str p)))
