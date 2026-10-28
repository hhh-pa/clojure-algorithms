(ns clojure-algorithms.matrix.sherman-morrison
  "Sherman-Morrison rank-one update of an inverse matrix.
  https://en.wikipedia.org/wiki/Sherman%E2%80%93Morrison_formula"
  (:require [clojure.string :as str]))

(defrecord Matrix [row column array])

(defn make-Matrix
  "Builds a row x column matrix filled with default-value (0)."
  ([row column] (make-Matrix row column 0))
  ([row column default-value]
   (->Matrix row
             column
             (mapv (fn [_] (vec (repeat column default-value))) (range row)))))

(defn Matrix-str
  "Renders the matrix with right-aligned columns."
  [self]
  (let [{:keys [row column array]} self
        width (reduce max 0 (map (comp count str) (apply concat array)))]
    (str (str "Matrix consist of " row " rows and " column " columns\n")
         (str/join "\n"
                   (map (fn [row-vector]
                          (str "["
                               (str/join ", "
                                         (map #(format (str "%" width "s") (str %))
                                              row-vector))
                               "]"))
                        array)))))

(defn Matrix-repr
  "Same as Matrix-str, mirroring upstream."
  [self]
  (Matrix-str self))

(defn Matrix-validate-indices
  "True when loc is a [row col] pair inside the matrix."
  [self loc]
  (boolean (and (sequential? loc)
                (= 2 (count loc))
                (let [[r c] loc]
                  (and (<= 0 r (dec (:row self)))
                       (<= 0 c (dec (:column self))))))))

(defn Matrix-getitem
  "Returns the element at [row col]."
  [self loc]
  (assert (Matrix-validate-indices self loc))
  (get-in (:array self) loc))

(defn Matrix-setitem
  "Returns a new matrix with value stored at [row col].
  Upstream mutates the matrix in place and returns nothing."
  [self loc value]
  (assert (Matrix-validate-indices self loc))
  (assoc-in self [:array (first loc) (second loc)] value))

(defn Matrix-add
  "Adds two matrices of equal size."
  [self another]
  (assert (instance? Matrix another))
  (assert (= (:row self) (:row another)))
  (assert (= (:column self) (:column another)))
  (->Matrix (:row self)
            (:column self)
            (mapv (fn [ra rb] (mapv +' ra rb))
                  (:array self)
                  (:array another))))

(defn Matrix-neg
  "Negates the matrix."
  [self]
  (->Matrix (:row self)
            (:column self)
            (mapv (fn [row] (mapv -' row)) (:array self))))

(defn Matrix-sub
  "Subtracts another matrix from this one."
  [self another]
  (Matrix-add self (Matrix-neg another)))

(defn Matrix-mul
  "Multiplies by a scalar or by another matrix."
  [self another]
  (cond (or (integer? another) (float? another))
        (->Matrix (:row self)
                  (:column self)
                  (mapv (fn [row] (mapv (fn [v] (*' v another)) row))
                        (:array self)))
        (instance? Matrix another)
        (do
          (assert (= (:column self) (:row another)))
          (let [cols (apply mapv vector (:array another))]
            (->Matrix (:row self)
                      (:column another)
                      (mapv (fn [row]
                              (mapv (fn [col] (reduce +' 0 (map *' row col)))
                                    cols))
                            (:array self)))))
        :else
        (throw (ex-info (str "Unsupported type given for another ("
                             (type another) ")")
                        {:type :TypeError}))))

(defn Matrix-transpose
  "Transposes the matrix."
  [self]
  (->Matrix (:column self)
            (:row self)
            (apply mapv vector (:array self))))

(defn Matrix-sherman-morrison
  "Applies the Sherman-Morrison formula: returns (A + u v^T)^-1 where
  this matrix is A^-1, or nil when it cannot be computed."
  [self u v]
  (assert (instance? Matrix u))
  (assert (instance? Matrix v))
  (assert (= (:row self) (:column self) (:row u) (:row v)))
  (assert (= 1 (:column u) (:column v)))
  (let [vt (Matrix-transpose v)
        factor (inc (Matrix-getitem (Matrix-mul (Matrix-mul vt self) u) [0 0]))]
    (when-not (zero? factor)
      (Matrix-sub self
                  (Matrix-mul (Matrix-mul (Matrix-mul self u)
                                          (Matrix-mul vt self))
                              (/ 1.0 factor))))))

(defmethod print-method Matrix [m w]
  (.write ^java.io.Writer w (if *print-readably* (Matrix-repr m) (Matrix-str m))))
