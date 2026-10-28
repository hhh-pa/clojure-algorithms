(ns clojure-algorithms.matrix.sherman-morrison
  "Port of matrix/sherman_morrison.py"
  (:require [clojure.string :as str]))

(declare make-Matrix Matrix-str Matrix-repr Matrix-validate-indices Matrix-getitem Matrix-setitem Matrix-add Matrix-neg Matrix-sub Matrix-mul Matrix-transpose Matrix-sherman-morrison)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-add py-repeat-vec py-mul py-mod py-neg py-min py-min-by py-max py-max-by py-float-str py-quote py-repr py-str py-type)

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

(defn- py-type [x]
  (cond (nil? x) :none-type
        (boolean? x) :bool
        (integer? x) :int
        (float? x) :float
        (string? x) :str
        (set? x) :set
        (map? x) :dict
        (sequential? x) :list
        :else (class x)))

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
