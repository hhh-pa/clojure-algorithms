(ns clojure-algorithms.matrix.matrix-class
  "An object-style matrix type with common operations and information."
  (:require [clojure.string :as str]))

(defrecord Matrix [rows])

(declare Matrix-cofactors Matrix-dot-product)

(def ^:private construct-error-message
  (str "Matrices must be formed from a list of zero or more lists containing "
       "at least one and the same number of values, each of which must be of "
       "type int or float."))

(defn- valid-value?
  [v]
  (or (integer? v) (float? v)))

(defn make-Matrix
  "Builds a Matrix from rows, a sequence of equally long sequences of
  ints and/or floats. An empty input yields the empty matrix."
  [rows]
  (let [error (ex-info construct-error-message {:type :TypeError})]
    (cond (not (sequential? rows))
          (throw error)
          (empty? rows)
          (->Matrix [])
          :else
          (let [lengths (map count rows)]
            (when (or (not (every? sequential? rows))
                      (zero? (first lengths))
                      (not (apply = lengths))
                      (not (every? valid-value? (mapcat seq rows))))
              (throw error))
            (->Matrix (mapv vec rows))))))

(defn Matrix-columns
  "Returns the columns of the matrix."
  [self]
  (if (empty? (:rows self))
    []
    (apply mapv vector (:rows self))))

(defn Matrix-num-rows
  "Returns the number of rows."
  [self]
  (count (:rows self)))

(defn Matrix-num-columns
  "Returns the number of columns."
  [self]
  (count (first (:rows self))))

(defn Matrix-order
  "Returns [rows columns]."
  [self]
  [(Matrix-num-rows self) (Matrix-num-columns self)])

(defn Matrix-is-square
  "True when the matrix has as many rows as columns."
  [self]
  (= (Matrix-num-rows self) (Matrix-num-columns self)))

(defn Matrix-identity
  "Returns the identity matrix with as many rows as this matrix."
  [self]
  (let [n (Matrix-num-rows self)]
    (make-Matrix (mapv (fn [r]
                         (mapv (fn [c] (if (= r c) 1 0)) (range n)))
                       (range n)))))

(defn Matrix-determinant
  "Returns the determinant: 0 for non-square matrices, 1 for the empty
  one, cofactor expansion otherwise."
  [self]
  (let [[r _c] (Matrix-order self)]
    (cond (not (Matrix-is-square self)) 0
          (zero? r) 1
          (= 1 r) (int (get-in (:rows self) [0 0]))
          (= 2 r) (let [[[a b] [d e]] (:rows self)]
                    (int (- (* a e) (* b d))))
          :else (reduce +'
                        0
                        (map-indexed (fn [j v]
                                       (* v (get-in (:rows (Matrix-cofactors self))
                                                    [0 j])))
                                     (first (:rows self)))))))

(defn Matrix-is-invertable
  "True when the determinant is non-zero."
  [self]
  (not (zero? (Matrix-determinant self))))

(defn Matrix-get-minor
  "Returns the determinant of the matrix with row and column removed."
  [self row column]
  (let [sub (vec (keep-indexed
                  (fn [i r]
                    (when (not= i row)
                      (vec (keep-indexed (fn [j v]
                                           (when (not= j column) v))
                                         r))))
                  (:rows self)))]
    (Matrix-determinant (make-Matrix sub))))

(defn Matrix-get-cofactor
  "Returns the signed minor for row and column."
  [self row column]
  (let [m (Matrix-get-minor self row column)]
    (if (even? (+ row column)) m (- m))))

(defn Matrix-minors
  "Returns the matrix of minors."
  [self]
  (let [[r c] (Matrix-order self)]
    (make-Matrix (mapv (fn [i]
                         (mapv (fn [j] (Matrix-get-minor self i j))
                               (range c)))
                       (range r)))))

(defn Matrix-cofactors
  "Returns the matrix of cofactors."
  [self]
  (let [minors (:rows (Matrix-minors self))
        [r c] (Matrix-order self)]
    (make-Matrix (mapv (fn [i]
                         (mapv (fn [j]
                                 (let [v (get-in minors [i j])]
                                   (if (even? (+ i j)) v (- v))))
                               (range c)))
                       (range r)))))

(defn Matrix-adjugate
  "Returns the transposed cofactor matrix."
  [self]
  (let [cf (:rows (Matrix-cofactors self))
        [r c] (Matrix-order self)]
    (make-Matrix (mapv (fn [i] (mapv (fn [j] (get-in cf [j i])) (range c)))
                       (range r)))))

(defn Matrix-mul
  "Multiplies the matrix by a scalar (truncating products to int, as
  upstream) or by another matrix."
  [self other]
  (cond (or (integer? other) (float? other))
        (make-Matrix (mapv (fn [row]
                             (mapv (fn [element] (int (* element other))) row))
                           (:rows self)))
        (instance? Matrix other)
        (do
          (when (not= (Matrix-num-columns self) (Matrix-num-rows other))
            (throw (ex-info (str "The number of columns in the first matrix must "
                                 "be equal to the number of rows in the second")
                            {:type :ValueError})))
          (let [cols (Matrix-columns other)]
            (make-Matrix (mapv (fn [row]
                                 (mapv (fn [col] (Matrix-dot-product row col))
                                       cols))
                               (:rows self)))))
        :else
        (throw (ex-info (str "A Matrix can only be multiplied by an int, float, "
                             "or another matrix")
                        {:type :TypeError}))))

(defn Matrix-inverse
  "Returns the inverse matrix. Throws when the determinant is zero."
  [self]
  (let [d (Matrix-determinant self)]
    (when (zero? d)
      (throw (ex-info "Only matrices with a non-zero determinant have an inverse"
                      {:type :TypeError})))
    (Matrix-mul (Matrix-adjugate self) (/ 1.0 d))))

(defn Matrix-repr
  "Returns the rows rendered as a Python-style nested list."
  [self]
  (str "[" (str/join ", " (map (fn [row]
                                 (str "[" (str/join ", " (map str row)) "]"))
                               (:rows self)))
       "]"))

(defn Matrix-str
  "Returns the human-readable rendering of the matrix."
  [self]
  (let [rows (:rows self)]
    (cond (zero? (count rows))
          "[]"
          (= 1 (count rows))
          ;; Mirrors upstream, which joins the characters of str(row).
          (let [rendered (str "[" (str/join ", " (map str (first rows))) "]")]
            (str "[[" (str/join ". " (map str rendered)) "]]"))
          :else
          (str "[" (str/join "\n " (map (fn [row]
                                          (str "[" (str/join ". " (map str row))
                                               ".]"))
                                        rows))
               "]"))))

(defn Matrix-add-row
  "Returns a new matrix with row added at the end, or at position when
  given. Upstream mutates the matrix in place and returns nothing."
  ([self row] (Matrix-add-row self row nil))
  ([self row position]
   (let [type-error (ex-info "Row must be a list containing all ints and/or floats"
                             {:type :TypeError})]
     (when (or (not (sequential? row)) (not (every? valid-value? row)))
       (throw type-error))
     (when (not= (count row) (Matrix-num-columns self))
       (throw (ex-info "Row must be equal in length to the other rows in the matrix"
                       {:type :ValueError})))
     (let [rows (:rows self)
           v (vec row)]
       (->Matrix (if (nil? position)
                   (conj rows v)
                   (vec (concat (take position rows) [v] (drop position rows)))))))))

(defn Matrix-add-column
  "Returns a new matrix with column added at the end, or at position
  when given. Upstream mutates the matrix in place and returns nothing."
  ([self column] (Matrix-add-column self column nil))
  ([self column position]
   (let [type-error (ex-info "Column must be a list containing all ints and/or floats"
                             {:type :TypeError})]
     (when (or (not (sequential? column)) (not (every? valid-value? column)))
       (throw type-error))
     (when (not= (count column) (Matrix-num-rows self))
       (throw (ex-info "Column must be equal in length to the other columns in the matrix"
                       {:type :ValueError})))
     (->Matrix (if (nil? position)
                 (mapv (fn [row v] (conj row v)) (:rows self) column)
                 (mapv (fn [row v]
                         (vec (concat (take position row) [v] (drop position row))))
                       (:rows self)
                       column))))))

(defn Matrix-eq
  "True when other is a Matrix with equal rows."
  [self other]
  (boolean (and (instance? Matrix other) (= (:rows self) (:rows other)))))

(defn Matrix-ne
  "The negation of Matrix-eq."
  [self other]
  (not (Matrix-eq self other)))

(defn Matrix-neg
  "Returns the negated matrix."
  [self]
  (Matrix-mul self -1))

(defn Matrix-add
  "Adds two matrices of the same order."
  [self other]
  (when (not= (Matrix-order self) (Matrix-order other))
    (throw (ex-info "Addition requires matrices of the same order"
                    {:type :ValueError})))
  (make-Matrix (mapv (fn [ra rb] (mapv +' ra rb))
                     (:rows self)
                     (:rows other))))

(defn Matrix-sub
  "Subtracts two matrices of the same order."
  [self other]
  (when (not= (Matrix-order self) (Matrix-order other))
    (throw (ex-info "Subtraction requires matrices of the same order"
                    {:type :ValueError})))
  (make-Matrix (mapv (fn [ra rb] (mapv -' ra rb))
                     (:rows self)
                     (:rows other))))

(defn Matrix-pow
  "Raises a square matrix to an integer power."
  [self other]
  (when-not (integer? other)
    (throw (ex-info "A Matrix can only be raised to the power of an int"
                    {:type :TypeError})))
  (when-not (Matrix-is-square self)
    (throw (ex-info "Only square matrices can be raised to a power"
                    {:type :ValueError})))
  (cond (zero? other)
        (Matrix-identity self)
        (neg? other)
        (if (Matrix-is-invertable self)
          (Matrix-pow (Matrix-inverse self) (- other))
          (throw (ex-info "Only invertable matrices can be raised to a negative power"
                          {:type :ValueError})))
        :else
        (reduce (fn [acc _] (Matrix-mul acc self)) self (range (dec other)))))

(defn Matrix-dot-product
  "Returns the dot product of a row and a column."
  [row column]
  (reduce +' 0 (map *' row column)))

(defmethod print-method Matrix [m w]
  (.write ^java.io.Writer w (if *print-readably* (Matrix-repr m) (Matrix-str m))))
