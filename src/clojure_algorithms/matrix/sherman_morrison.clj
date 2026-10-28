(ns clojure-algorithms.matrix.sherman-morrison
  "Port of matrix/sherman_morrison.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare make-Matrix Matrix-str Matrix-repr Matrix-validate-indices Matrix-getitem Matrix-setitem Matrix-add Matrix-neg Matrix-sub Matrix-mul Matrix-transpose Matrix-sherman-morrison)

(defrecord Matrix [array])

(defn make-Matrix "Constructor for Matrix. <class Matrix>\nMatrix structure."
  ([row column] (make-Matrix row column 0))
  ([row column default-value] (let [self (atom {})] (try (let [__d1 (vec [row column])] (swap! self (fn [__c] (py-assoc-in __c [:row] (nth __d1 0)))) (swap! self (fn [__c] (py-assoc-in __c [:column] (nth __d1 1))))) (swap! self (fn [__c] (py-assoc-in __c [:array] (vec (for [--v (py-iter (range row))] (vec (for [--v (py-iter (range column))] default-value))))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Matrix @self))))

(defn Matrix-str "<method Matrix.__str__>\nReturn string representation of this matrix."
  [self] (let [max-element-length (atom nil) obj (atom nil) row-vector (atom nil) s (atom nil) string-format-identifier (atom nil)] (try (reset! s (str "Matrix consist of " (py-str (:row @self)) " rows and " (py-str (:column @self)) " columns\n")) (reset! max-element-length 0) (doseq [__i1 (py-iter (:array @self))] (reset! row-vector __i1) (doseq [__i2 (py-iter @row-vector)] (reset! obj __i2) (reset! max-element-length (py-max [@max-element-length (count (py-str @obj))])))) (reset! string-format-identifier (str "%" (py-str @max-element-length) "s")) (letfn [(single-line [row-vector] (let [line (atom nil)] (try (reset! line "[") (swap! line py-add (clojure.string/join ", " (map py-str (doall (for [obj (py-iter row-vector)] (py-mod @string-format-identifier [obj])))))) (swap! line py-add "]") (throw (ex-info "__return__" {:__control__ true :__return__ true :value @line})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))] (swap! s py-add (clojure.string/join "\n" (map py-str (doall (for [row-vector (py-iter (:array @self))] (single-line row-vector)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @s}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-repr
  [self] (py-str @self))

(defn Matrix-validate-indices "<method Matrix.validate_indicies>\nCheck if given indices are valid to pick element from matrix.\nExample:\n>>> a = Matrix(2, 6, 0)\n>>> a.validate_indices((2, 7))\nFalse\n>>> a.validate_indices((0, 0))\nTrue"
  [self loc] (try (if (py-truthy (not (py-truthy (let [__b1 (or (sequential? loc) (sequential? loc))] (if (py-truthy __b1) (py-eq (count loc) 2) __b1))))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (do (if (py-truthy (not (py-truthy (let [__b8 (let [__c2 0 __c3 (py-get loc 0) __c4 (:row @self)] (and (py-le __c2 __c3) (py-lt __c3 __c4)))] (if (py-truthy __b8) (let [__c5 0 __c6 (py-get loc 1) __c7 (:column @self)] (and (py-le __c5 __c6) (py-lt __c6 __c7))) __b8))))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value true})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-getitem "<method Matrix.__getitem__>\nReturn array[row][column] where loc = (row, column).\nExample:\n>>> a = Matrix(3, 2, 7)\n>>> a[1, 0]\n7"
  [self loc] (try (assert (py-truthy (Matrix-validate-indices self loc))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get (:array @self) (py-get loc 0)) (py-get loc 1))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn Matrix-setitem "<method Matrix.__setitem__>\nSet array[row][column] = value where loc = (row, column).\nExample:\n>>> a = Matrix(2, 3, 1)\n>>> a[1, 2] = 51\n>>> a\nMatrix consist of 2 rows and 3 columns\n[ 1,  1,  1]\n[ 1,  1, 51]"
  [self loc value] (assert (py-truthy (Matrix-validate-indices self loc))) (swap! self (fn [__c] (py-assoc-in __c [:array (py-get loc 0) (py-get loc 1)] value))) nil)

(defn Matrix-add "<method Matrix.__add__>\nReturn self + another.\nExample:\n>>> a = Matrix(2, 1, -4)\n>>> b = Matrix(2, 1, 3)\n>>> a+b\nMatrix consist of 2 rows and 1 columns\n[-1]\n[-1]"
  [self another] (let [c (atom nil) r (atom nil) result (atom nil)] (try (assert (py-truthy (instance? Matrix another))) (assert (py-truthy (py-eq (:row @self) (:row another)))) (assert (py-truthy (py-eq (:column @self) (:column another)))) (reset! result (make-Matrix (:row @self) (:column @self))) (doseq [__i1 (py-iter (range (:row @self)))] (reset! r __i1) (doseq [__i2 (py-iter (range (:column @self)))] (reset! c __i2) (swap! result (fn [__c] (py-assoc-in __c [@r @c] (py-add (py-get-in @self [@r @c]) (py-get-in another [@r @c]))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-neg "<method Matrix.__neg__>\nReturn -self.\nExample:\n>>> a = Matrix(2, 2, 3)\n>>> a[0, 1] = a[1, 0] = -2\n>>> -a\nMatrix consist of 2 rows and 2 columns\n[-3,  2]\n[ 2, -3]"
  [self] (let [c (atom nil) r (atom nil) result (atom nil)] (try (reset! result (make-Matrix (:row @self) (:column @self))) (doseq [__i1 (py-iter (range (:row @self)))] (reset! r __i1) (doseq [__i2 (py-iter (range (:column @self)))] (reset! c __i2) (swap! result (fn [__c] (py-assoc-in __c [@r @c] (py-neg (py-get-in @self [@r @c]))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-sub
  [self another] (py-add @self (py-neg another)))

(defn Matrix-mul "<method Matrix.__mul__>\nReturn self * another.\nExample:\n>>> a = Matrix(2, 3, 1)\n>>> a[0,2] = a[1,2] = 3\n>>> a * -2\nMatrix consist of 2 rows and 3 columns\n[-2, -2, -6]\n[-2, -2, -6]"
  [self another] (let [c (atom nil) i (atom nil) msg (atom nil) r (atom nil) result (atom nil)] (try (if (py-truthy (let [__b1 (integer? another)] (if (py-truthy __b1) __b1 (number? another)))) (do (reset! result (make-Matrix (:row @self) (:column @self))) (doseq [__i2 (py-iter (range (:row @self)))] (reset! r __i2) (doseq [__i3 (py-iter (range (:column @self)))] (reset! c __i3) (swap! result (fn [__c] (py-assoc-in __c [@r @c] (py-mul (py-get-in @self [@r @c]) another)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result}))) (do (if (py-truthy (instance? Matrix another)) (do (assert (py-truthy (py-eq (:column @self) (:row another)))) (reset! result (make-Matrix (:row @self) (:column another))) (doseq [__i4 (py-iter (range (:row @self)))] (reset! r __i4) (doseq [__i5 (py-iter (range (:column another)))] (reset! c __i5) (doseq [__i6 (py-iter (range (:column @self)))] (reset! i __i6) (swap! result (fn [__c] (py-assoc-in __c [@r @c] (py-add (py-get-in __c [@r @c]) (py-mul (py-get-in @self [@r @i]) (py-get-in another [@i @c]))))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result}))) (do (reset! msg (str "Unsupported type given for another (" (py-str (py-type another)) ")")) (throw (ex-info (py-str @msg) {:type :TypeError})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-transpose "<method Matrix.transpose>\nReturn self^T.\nExample:\n>>> a = Matrix(2, 3)\n>>> for r in range(2):\n...     for c in range(3):\n...             a[r,c] = r*c\n...\n>>> a.transpose()\nMatrix consist of 3 rows and 2 columns\n[0, 0]\n[0, 1]\n[0, 2]"
  [self] (let [c (atom nil) r (atom nil) result (atom nil)] (try (reset! result (make-Matrix (:column @self) (:row @self))) (doseq [__i1 (py-iter (range (:row @self)))] (reset! r __i1) (doseq [__i2 (py-iter (range (:column @self)))] (reset! c __i2) (swap! result (fn [__c] (py-assoc-in __c [@c @r] (py-get-in @self [@r @c])))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Matrix-sherman-morrison "<method Matrix.sherman_morrison>\nApply Sherman-Morrison formula in O(n^2).\nTo learn this formula, please look this:\nhttps://en.wikipedia.org/wiki/Sherman%E2%80%93Morrison_formula\nThis method returns (A + uv^T)^(-1) where A^(-1) is self. Returns None if it's\nimpossible to calculate.\nWarning: This method doesn't check if self is invertible.\n    Make sure self is invertible before execute this method.\nExample:\n>>> ainv = Matrix(3, 3, 0)\n>>> for i in range(3): ainv[i,i] = 1\n...\n>>> u = Matrix(3, 1, 0)\n>>> u[0,0], u[1,0], u[2,0] = 1, 2, -3\n>>> v = Matrix(3, 1, 0)\n>>> v[0,0], v[1,0], v[2,0] = 4, -2, 5\n>>> ainv.sherman_morrison(u, v)\nMatrix consist of 3 rows and 3 columns\n[  1.2857142857142856, -0.14285714285714285,   0.3571428571428571]\n[  0.5714285714285714,   0.7142857142857143,   0.7142857142857142]\n[ -0.8571428571428571,  0.42857142857142855,  -0.0714285714285714]"
  [& _args]
  ;; stub: original requires numpy-attr:transpose; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:transpose" {:type :UnsupportedOperation})))

(defmethod print-method Matrix [o w] (.write w (if *print-readably* (Matrix-repr (atom o)) (Matrix-str (atom o)))))

(defmethod py-dunder [:getitem Matrix] [_ a b] (Matrix-getitem (atom a) b))

(defmethod py-dunder [:setitem Matrix] [_ a k v] (let [__s (atom a)] (Matrix-setitem __s k v) @__s))

(defmethod py-dunder [:add Matrix] [_ a b] (Matrix-add (atom a) b))

(defmethod py-dunder [:neg Matrix] [_ a] (Matrix-neg (atom a)))

(defmethod py-dunder [:sub Matrix] [_ a b] (Matrix-sub (atom a) b))

(defmethod py-dunder [:mul Matrix] [_ a b] (Matrix-mul (atom a) b))

;; NOTE: Matrix.sherman_morrison stubbed (numpy-attr:transpose)
