(ns clojure-algorithms.maths.dual-number-automatic-differentiation
  "Automatic differentiation using dual numbers."
  (:require [clojure.string :as str]))

(defrecord Dual [real duals])

(defn make-Dual
  "Build a Dual with the given real part; rank is either an order (int)
  producing that many unit duals, or an explicit duals collection."
  [real rank]
  (if (integer? rank)
    (->Dual real (vec (repeat rank 1)))
    (->Dual real (vec rank))))

(defn Dual-repr
  "String representation of a Dual number."
  [self]
  (let [s (str/join "+" (map-indexed (fn [n dual] (str dual "E" (inc n)))
                                     (:duals self)))]
    (str (:real self) "+" s)))

(defn Dual-reduce
  "Drop trailing zero duals."
  [self]
  (loop [cur (:duals self)]
    (if (and (seq cur) (zero? (peek cur)))
      (recur (pop cur))
      (->Dual (:real self) cur))))

(defn- pad-with-ones
  [v n]
  (vec (concat v (repeat (- n (count v)) 1))))

(defn Dual-add
  "Add a Dual and another Dual or a plain number."
  [self other]
  (if (not (instance? Dual other))
    (->Dual (+ (:real self) other) (:duals self))
    (let [n (max (count (:duals self)) (count (:duals other)))
          s (pad-with-ones (:duals self) n)
          o (pad-with-ones (:duals other) n)]
      (->Dual (+ (:real self) (:real other)) (mapv + s o)))))

(defn Dual-mul
  "Multiply a Dual by another Dual or a plain number."
  [self other]
  (if (not (instance? Dual other))
    (->Dual (* (:real self) other) (mapv #(* % other) (:duals self)))
    (let [s (:duals self)
          o (:duals other)
          acc (transient (vec (repeat (+ (count s) (count o) 1) 0)))
          acc (reduce (fn [a [i si]]
                        (reduce (fn [a [j oj]]
                                  (let [k (+ i j 1)]
                                    (assoc! a k (+ (get a k) (* si oj)))))
                                a (map-indexed vector o)))
                      acc (map-indexed vector s))
          acc (reduce (fn [a [k sk]]
                        (assoc! a k (+ (get a k) (* sk (:real other)))))
                      acc (map-indexed vector s))
          acc (reduce (fn [a [k ok]]
                        (assoc! a k (+ (get a k) (* ok (:real self)))))
                      acc (map-indexed vector o))]
      (->Dual (* (:real self) (:real other)) (persistent! acc)))))

(defn Dual-sub
  "Subtract another Dual or a plain number from a Dual."
  [self other]
  (Dual-add self (if (instance? Dual other) (Dual-mul other -1) (- other))))

(defn Dual-truediv
  "Divide a Dual by a plain number."
  [self other]
  (if (instance? Dual other)
    (throw (ex-info "Cannot divide a Dual by a Dual" {:type :ValueError}))
    (do (when (zero? other)
          (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
        (->Dual (/ (double (:real self)) (double other))
                (mapv #(/ (double %) (double other)) (:duals self))))))

(defn- floordiv-num
  [a b]
  (if (and (integer? a) (integer? b))
    (let [q (quot a b)
          r (rem a b)]
      (if (or (zero? r) (= (neg? a) (neg? b))) q (dec q)))
    (Math/floor (/ (double a) (double b)))))

(defn Dual-floordiv
  "Floor-divide a Dual by a plain number."
  [self other]
  (if (instance? Dual other)
    (throw (ex-info "Cannot floor-divide a Dual by a Dual" {:type :ValueError}))
    (do (when (zero? other)
          (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
        (->Dual (floordiv-num (:real self) other)
                (mapv #(floordiv-num % other) (:duals self))))))

(defn Dual-pow
  "Raise a Dual to a non-negative integer power."
  [self n]
  (when (or (not (integer? n)) (neg? n))
    (throw (ex-info "power must be a positive integer" {:type :ValueError})))
  (cond (zero? n) 1
        (= n 1) self
        :else (loop [x self
                     i (dec n)]
                (if (zero? i) x (recur (Dual-mul x self) (dec i))))))

(defn- factorial
  [n]
  (reduce * (range 1 (inc n))))

(defn differentiate
  "Compute the order-th derivative of func at position via dual numbers."
  [func position order]
  (when (not (ifn? func))
    (throw (ex-info "differentiate() requires a function as input for func"
                    {:type :ValueError})))
  (when (not (number? position))
    (throw (ex-info "differentiate() requires a float as input for position"
                    {:type :ValueError})))
  (when (not (integer? order))
    (throw (ex-info "differentiate() requires an int as input for order"
                    {:type :ValueError})))
  (let [result (func (make-Dual position 1))]
    (if (zero? order)
      (:real result)
      (* (nth (:duals result) (dec order)) (factorial order)))))

(def Dual---radd-- Dual-add)

(def Dual---rmul-- Dual-mul)

(defmethod print-method Dual [o w] (.write ^java.io.Writer w (Dual-repr o)))
