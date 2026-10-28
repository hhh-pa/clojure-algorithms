(ns clojure-algorithms.matrix.matrix-class
  "Port of matrix/matrix_class.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare make-Matrix Matrix-columns Matrix-num-rows Matrix-num-columns Matrix-order Matrix-is-square Matrix-identity Matrix-determinant Matrix-is-invertable Matrix-get-minor Matrix-get-cofactor Matrix-minors Matrix-cofactors Matrix-adjugate Matrix-inverse Matrix-repr Matrix-str Matrix-add-row Matrix-add-column Matrix-eq Matrix-ne Matrix-neg Matrix-add Matrix-sub Matrix-mul Matrix-pow Matrix-dot-product)

(defrecord Matrix [rows])

(defn make-Matrix "Constructor for Matrix. Matrix object generated from a 2D array where each element is an array representing\na row.\nRows can contain type int or float.\nCommon operations and information available.\n>>> rows = [\n...     [1, 2, 3],\n...     [4, 5, 6],\n...     [7, 8, 9]\n... ]\n>>> matrix = Matrix(rows)\n>>> print(matrix)\n[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]]\n\nMatrix rows and columns are available as 2D arrays\n>>> matrix.rows\n[[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n>>> matrix.columns()\n[[1, 4, 7], [2, 5, 8], [3, 6, 9]]\n\nOrder is returned as a tuple\n>>> matrix.order\n(3, 3)\n\nSquareness and invertability are represented as bool\n>>> matrix.is_square\nTrue\n>>> matrix.is_invertable()\nFalse\n\nIdentity, Minors, Cofactors and Adjugate are returned as Matrices.  Inverse can be\na Matrix or Nonetype\n>>> print(matrix.identity())\n[[1. 0. 0.]\n [0. 1. 0.]\n [0. 0. 1.]]\n>>> print(matrix.minors())\n[[-3. -6. -3.]\n [-6. -12. -6.]\n [-3. -6. -3.]]\n>>> print(matrix.cofactors())\n[[-3. 6. -3.]\n [6. -12. 6.]\n [-3. 6. -3.]]\n>>>  # won't be apparent due to the nature of the cofactor matrix\n>>> print(matrix.adjugate())\n[[-3. 6. -3.]\n [6. -12. 6.]\n [-3. 6. -3.]]\n>>> matrix.inverse()\nTraceback (most recent call last):\n    ...\nTypeError: Only matrices with a non-zero determinant have an inverse\n\nDeterminant is an int, float, or Nonetype\n>>> matrix.determinant()\n0\n\nNegation, scalar multiplication, addition, subtraction, multiplication and\nexponentiation are available and all return a Matrix\n>>> print(-matrix)\n[[-1. -2. -3.]\n [-4. -5. -6.]\n [-7. -8. -9.]]\n>>> matrix2 = matrix * 3\n>>> print(matrix2)\n[[3. 6. 9.]\n [12. 15. 18.]\n [21. 24. 27.]]\n>>> print(matrix + matrix2)\n[[4. 8. 12.]\n [16. 20. 24.]\n [28. 32. 36.]]\n>>> print(matrix - matrix2)\n[[-2. -4. -6.]\n [-8. -10. -12.]\n [-14. -16. -18.]]\n>>> print(matrix ** 3)\n[[468. 576. 684.]\n [1062. 1305. 1548.]\n [1656. 2034. 2412.]]\n\nMatrices can also be modified\n>>> matrix.add_row([10, 11, 12])\n>>> print(matrix)\n[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]\n [10. 11. 12.]]\n>>> matrix2.add_column([8, 16, 32])\n>>> print(matrix2)\n[[3. 6. 9. 8.]\n [12. 15. 18. 16.]\n [21. 24. 27. 32.]]\n>>> print(matrix *  matrix2)\n[[90. 108. 126. 136.]\n [198. 243. 288. 304.]\n [306. 378. 450. 472.]\n [414. 513. 612. 640.]]"
  [rows] (let [self (atom {}) cols (atom nil) error (atom nil) row (atom nil) value (atom nil)] (try (reset! error (ex-info (py-str "Matrices must be formed from a list of zero or more lists containing at least one and the same number of values, each of which must be of type int or float.") {:type :TypeError})) (if (py-truthy (not (py-eq (count rows) 0))) (do (reset! cols (count (py-get rows 0))) (when (py-truthy (py-eq @cols 0)) (throw @error)) (doseq [__i1 (py-iter rows)] (reset! row __i1) (when (py-truthy (not (py-eq (count @row) @cols))) (throw @error)) (doseq [__i2 (py-iter @row)] (reset! value __i2) (when (py-truthy (not (py-truthy (or (integer? @value) (number? @value))))) (throw @error)))) (swap! self (fn [__c] (py-assoc-in __c [:rows] rows)))) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] []))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Matrix @self)))

(defn Matrix-columns
  [self] (vec (for [i (py-iter (range (count (py-get (:rows @self) 0))))] (vec (for [row (py-iter (:rows @self))] (py-get row i))))))

(defn Matrix-num-rows
  [self] (count (:rows @self)))

(defn Matrix-num-columns
  [self] (count (py-get (:rows @self) 0)))

(defn Matrix-order
  [self] [(Matrix-num-rows self) (Matrix-num-columns self)])

(defn Matrix-is-square
  [self] (py-eq (py-get (Matrix-order self) 0) (py-get (Matrix-order self) 1)))

(defn Matrix-identity
  [self] (let [values (atom nil)] (try (reset! values (vec (for [row-num (py-iter (range (Matrix-num-rows self)))] (vec (for [column-num (py-iter (range (Matrix-num-rows self)))] (if (py-truthy (not (py-eq column-num row-num))) 0 1)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix @values)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-determinant
  [self] (try (when (py-truthy (not (py-truthy (Matrix-is-square self)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (when (py-truthy (py-eq (Matrix-order self) [0 0])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (when (py-truthy (py-eq (Matrix-order self) [1 1])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-int (py-get (py-get (:rows @self) 0) 0) nil)}))) (if (py-truthy (py-eq (Matrix-order self) [2 2])) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-int (py-sub (py-mul (py-get (py-get (:rows @self) 0) 0) (py-get (py-get (:rows @self) 1) 1)) (py-mul (py-get (py-get (:rows @self) 0) 1) (py-get (py-get (:rows @self) 1) 0))) nil)}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [column (py-iter (range (Matrix-num-columns self)))] (py-mul (py-get (py-get (:rows @self) 0) column) (py-get (py-get (:rows (Matrix-cofactors self)) 0) column)))))})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-is-invertable
  [self] (boolean (py-truthy (Matrix-determinant self))))

(defn Matrix-get-minor
  [self row column] (let [values (atom nil)] (try (reset! values (vec (for [other-row (py-iter (range (Matrix-num-rows self))) :when (py-truthy (not (py-eq other-row row)))] (vec (for [other-column (py-iter (range (Matrix-num-columns self))) :when (py-truthy (not (py-eq other-column column)))] (py-get (py-get (:rows @self) other-row) other-column)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Matrix-determinant (atom (make-Matrix @values)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-get-cofactor
  [self row column] (try (when (py-truthy (py-eq (py-mod (py-add row column) 2) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Matrix-get-minor self row column)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-neg 1) (Matrix-get-minor self row column))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-minors
  [self] (make-Matrix (vec (for [row (py-iter (range (Matrix-num-rows self)))] (vec (for [column (py-iter (range (Matrix-num-columns self)))] (Matrix-get-minor self row column)))))))

(defn Matrix-cofactors
  [self] (make-Matrix (vec (for [row (py-iter (range (Matrix-num-rows (atom (Matrix-minors self)))))] (vec (for [column (py-iter (range (Matrix-num-columns (atom (Matrix-minors self)))))] (if (py-truthy (py-eq (py-mod (py-add row column) 2) 0)) (py-get (py-get (:rows (Matrix-minors self)) row) column) (py-mul (py-get (py-get (:rows (Matrix-minors self)) row) column) (py-neg 1)))))))))

(defn Matrix-adjugate
  [self] (let [values (atom nil)] (try (reset! values (vec (for [row (py-iter (range (Matrix-num-rows self)))] (vec (for [column (py-iter (range (Matrix-num-columns self)))] (py-get (py-get (:rows (Matrix-cofactors self)) column) row)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix @values)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-inverse
  [self] (let [determinant (atom nil)] (try (reset! determinant (Matrix-determinant self)) (when (py-truthy (not (py-truthy @determinant))) (throw (ex-info (py-str "Only matrices with a non-zero determinant have an inverse") {:type :TypeError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (Matrix-adjugate self) (py-div 1 @determinant))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-repr
  [self] (py-str (:rows @self)))

(defn Matrix-str
  [self] (try (when (py-truthy (py-eq (Matrix-num-rows self) 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value "[]"}))) (when (py-truthy (py-eq (Matrix-num-rows self) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-add "[[" (clojure.string/join ". " (map py-str (py-str (py-get (:rows @self) 0))))) "]]")}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-add "[" (clojure.string/join "\n " (map py-str (vec (for [row (py-iter (:rows @self))] (py-add (py-add "[" (clojure.string/join ". " (map py-str (vec (for [value (py-iter row)] (py-str value)))))) ".]")))))) "]")})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-add-row
  ([self row] (Matrix-add-row self row nil))
  ([self row position] (let [type-error (atom nil) value (atom nil)] (reset! type-error (ex-info (py-str "Row must be a list containing all ints and/or floats") {:type :TypeError})) (when (py-truthy (not (py-truthy (sequential? row)))) (throw @type-error)) (doseq [__i1 (py-iter row)] (reset! value __i1) (when (py-truthy (not (py-truthy (or (integer? @value) (number? @value))))) (throw @type-error))) (when (py-truthy (not (py-eq (count row) (Matrix-num-columns self)))) (throw (ex-info (py-str "Row must be equal in length to the other rows in the matrix") {:type :ValueError}))) (if (py-truthy (nil? position)) (do (conj (vec (:rows @self)) row)) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] (vec (concat (seq (py-slice (:rows @self) 0 position nil)) (list row) (seq (py-slice (:rows @self) position nil nil))))))))) nil)))

(defn Matrix-add-column
  ([self column] (Matrix-add-column self column nil))
  ([self column position] (let [type-error (atom nil) value (atom nil)] (reset! type-error (ex-info (py-str "Column must be a list containing all ints and/or floats") {:type :TypeError})) (when (py-truthy (not (py-truthy (sequential? column)))) (throw @type-error)) (doseq [__i1 (py-iter column)] (reset! value __i1) (when (py-truthy (not (py-truthy (or (integer? @value) (number? @value))))) (throw @type-error))) (when (py-truthy (not (py-eq (count column) (Matrix-num-rows self)))) (throw (ex-info (py-str "Column must be equal in length to the other columns in the matrix") {:type :ValueError}))) (if (py-truthy (nil? position)) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] (vec (for [i (py-iter (range (Matrix-num-rows self)))] (py-add (py-get (:rows @self) i) [(py-get column i)]))))))) (do (swap! self (fn [__c] (py-assoc-in __c [:rows] (vec (for [i (py-iter (range (Matrix-num-rows self)))] (vec (concat (seq (py-slice (py-get (:rows @self) i) 0 position nil)) (list (py-get column i)) (seq (py-slice (py-get (:rows @self) i) position nil nil))))))))))) nil)))

(defn Matrix-eq
  [self other] (try (when (py-truthy (not (py-truthy (instance? Matrix other)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value :py-not-implemented}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq (:rows @self) (:rows other))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-ne
  [self other] (not (py-truthy (py-eq @self other))))

(defn Matrix-neg
  [self] (py-mul @self (py-neg 1)))

(defn Matrix-add
  [self other] (try (when (py-truthy (not (py-eq (Matrix-order self) (Matrix-order (atom other))))) (throw (ex-info (py-str "Addition requires matrices of the same order") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [i (py-iter (range (Matrix-num-rows self)))] (vec (for [j (py-iter (range (Matrix-num-columns self)))] (py-add (py-get (py-get (:rows @self) i) j) (py-get (py-get (:rows other) i) j)))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-sub
  [self other] (try (when (py-truthy (not (py-eq (Matrix-order self) (Matrix-order (atom other))))) (throw (ex-info (py-str "Subtraction requires matrices of the same order") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [i (py-iter (range (Matrix-num-rows self)))] (vec (for [j (py-iter (range (Matrix-num-columns self)))] (py-sub (py-get (py-get (:rows @self) i) j) (py-get (py-get (:rows other) i) j)))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-mul
  [self other] (try (if (py-truthy (let [__b1 (integer? other)] (if (py-truthy __b1) __b1 (number? other)))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [row (py-iter (:rows @self))] (vec (for [element (py-iter row)] (py-int (py-mul element other) nil))))))}))) (do (if (py-truthy (instance? Matrix other)) (do (when (py-truthy (not (py-eq (Matrix-num-columns self) (Matrix-num-rows (atom other))))) (throw (ex-info (py-str "The number of columns in the first matrix must be equal to the number of rows in the second") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (make-Matrix (vec (for [row (py-iter (:rows @self))] (vec (for [column (py-iter (Matrix-columns (atom other)))] (Matrix-dot-product row column))))))}))) (do (throw (ex-info (py-str "A Matrix can only be multiplied by an int, float, or another matrix") {:type :TypeError})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-pow
  [self other] (let [--v (atom nil) result (atom nil)] (try (when (py-truthy (not (py-truthy (integer? other)))) (throw (ex-info (py-str "A Matrix can only be raised to the power of an int") {:type :TypeError}))) (when (py-truthy (not (py-truthy (Matrix-is-square self)))) (throw (ex-info (py-str "Only square matrices can be raised to a power") {:type :ValueError}))) (when (py-truthy (py-eq other 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Matrix-identity self)}))) (when (py-truthy (py-lt other 0)) (when (py-truthy (Matrix-is-invertable self)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-pow (Matrix-inverse self) (py-neg other))}))) (throw (ex-info (py-str "Only invertable matrices can be raised to a negative power") {:type :ValueError}))) (reset! result @self) (doseq [__i1 (py-iter (range (py-sub other 1)))] (reset! --v __i1) (swap! result py-mul @self)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-dot-product
  [row column] (reduce +' 0 (doall (for [i (py-iter (range (count row)))] (py-mul (py-get row i) (py-get column i))))))

(defmethod print-method Matrix [o w] (.write w (if *print-readably* (Matrix-repr (atom o)) (Matrix-str (atom o)))))

(defmethod py-dunder [:eq Matrix] [_ a b] (Matrix-eq (atom a) b))

(defmethod py-dunder [:neg Matrix] [_ a] (Matrix-neg (atom a)))

(defmethod py-dunder [:add Matrix] [_ a b] (Matrix-add (atom a) b))

(defmethod py-dunder [:sub Matrix] [_ a b] (Matrix-sub (atom a) b))

(defmethod py-dunder [:mul Matrix] [_ a b] (Matrix-mul (atom a) b))

(defmethod py-dunder [:pow Matrix] [_ a b] (Matrix-pow (atom a) b))
