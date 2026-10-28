(ns clojure-algorithms.linear-programming.simplex
  "Python implementation of the simplex algorithm for solving linear programs in\ntabular form with\n- `>=`, `<=`, and `=` constraints and\n- each variable `x1, x2, ...>= 0`.\n\nSee https://gist.github.com/imengus/f9619a568f7da5bc74eaf20169a24d98 for how to\nconvert linear programs to simplex tableaus, and the steps taken in the simplex\nalgorithm.\n\nResources:\nhttps://en.wikipedia.org/wiki/Simplex_algorithm\nhttps://tinyurl.com/simplex4beginners"
  (:require [clojure.string :as str]))

(declare make-Tableau Tableau-generate-col-titles Tableau-find-pivot Tableau-pivot Tableau-change-stage Tableau-run-simplex Tableau-interpret-tableau Tableau-maxiter)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-float-str py-quote py-repr py-str)

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

(defrecord Tableau [tableau n-stages n-slack objectives col-titles row-idx col-idx stop-iter n-rows n-artificial-vars])

(defn make-Tableau "Constructor for Tableau. Operate on simplex tableaus\n\n>>> Tableau(np.array([[-1,-1,0,0,1],[1,3,1,0,4],[3,1,0,1,4]]), 2, 2)\nTraceback (most recent call last):\n...\nTypeError: Tableau must have type float64\n\n>>> Tableau(np.array([[-1,-1,0,0,-1],[1,3,1,0,4],[3,1,0,1,4.]]), 2, 2)\nTraceback (most recent call last):\n...\nValueError: RHS must be > 0\n\n>>> Tableau(np.array([[-1,-1,0,0,1],[1,3,1,0,4],[3,1,0,1,4.]]), -2, 2)\nTraceback (most recent call last):\n...\nValueError: number of (artificial) variables must be a natural number"
  [& _args]
  ;; stub: original requires np,numpy-attr:all,numpy-attr:dtype,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:all,numpy-attr:dtype,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn Tableau-generate-col-titles "Generate column titles for tableau of specific dimensions\n\n>>> Tableau(np.array([[-1,-1,0,0,1],[1,3,1,0,4],[3,1,0,1,4.]]),\n... 2, 0).generate_col_titles()\n['x1', 'x2', 's1', 's2', 'RHS']\n\n>>> Tableau(np.array([[-1,-1,0,0,1],[1,3,1,0,4],[3,1,0,1,4.]]),\n... 2, 2).generate_col_titles()\n['x1', 'x2', 'RHS']"
  [self] (let [args (atom nil) i (atom nil) j (atom nil) string-starts (atom nil) titles (atom nil)] (try (reset! args [(:n-vars @self) (:n-slack @self)]) (reset! string-starts ["x" "s"]) (reset! titles []) (doseq [__i1 (py-iter (range 2))] (reset! i __i1) (doseq [__i2 (py-iter (range (py-get @args @i)))] (reset! j __i2) (swap! titles conj (py-add (py-get @string-starts @i) (py-str (py-add @j 1)))))) (swap! titles conj "RHS") (throw (ex-info "__return__" {:__control__ true :__return__ true :value @titles})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Tableau-find-pivot "Finds the pivot row and column.\n>>> tuple(int(x) for x in Tableau(np.array([[-2,1,0,0,0], [3,1,1,0,6],\n... [1,2,0,1,7.]]), 2, 0).find_pivot())\n(1, 0)"
  [& _args]
  ;; stub: original requires np,numpy-attr:argmax; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:argmax" {:type :UnsupportedOperation})))

(defn Tableau-pivot "Pivots on value on the intersection of pivot row and column.\n\n>>> Tableau(np.array([[-2,-3,0,0,0],[1,3,1,0,4],[3,1,0,1,4.]]),\n... 2, 2).pivot(1, 0).tolist()\n... # doctest: +NORMALIZE_WHITESPACE\n[[0.0, 3.0, 2.0, 0.0, 8.0],\n[1.0, 3.0, 1.0, 0.0, 4.0],\n[0.0, -8.0, -3.0, 1.0, -8.0]]"
  [& _args]
  ;; stub: original requires np,numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:copy" {:type :UnsupportedOperation})))

(defn Tableau-change-stage "Exits first phase of the two-stage method by deleting artificial\nrows and columns, or completes the algorithm if exiting the standard\ncase.\n\n>>> Tableau(np.array([\n... [3, 3, -1, -1, 0, 0, 4],\n... [2, 1, 0, 0, 0, 0, 0.],\n... [1, 2, -1, 0, 1, 0, 2],\n... [2, 1, 0, -1, 0, 1, 2]\n... ]), 2, 2).change_stage().tolist()\n... # doctest: +NORMALIZE_WHITESPACE\n[[2.0, 1.0, 0.0, 0.0, 0.0],\n[1.0, 2.0, -1.0, 0.0, 2.0],\n[2.0, 1.0, 0.0, -1.0, 2.0]]"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Tableau-run-simplex "Operate on tableau until objective function cannot be\nimproved further.\n\n# Standard linear program:\nMax:  x1 +  x2\nST:   x1 + 3x2 <= 4\n     3x1 +  x2 <= 4\n>>> {key: float(value) for key, value in Tableau(np.array([[-1,-1,0,0,0],\n... [1,3,1,0,4],[3,1,0,1,4.]]), 2, 0).run_simplex().items()}\n{'P': 2.0, 'x1': 1.0, 'x2': 1.0}\n\n# Standard linear program with 3 variables:\nMax: 3x1 +  x2 + 3x3\nST:  2x1 +  x2 +  x3 ≤ 2\n      x1 + 2x2 + 3x3 ≤ 5\n     2x1 + 2x2 +  x3 ≤ 6\n>>> {key: float(value) for key, value in Tableau(np.array([\n... [-3,-1,-3,0,0,0,0],\n... [2,1,1,1,0,0,2],\n... [1,2,3,0,1,0,5],\n... [2,2,1,0,0,1,6.]\n... ]),3,0).run_simplex().items()} # doctest: +ELLIPSIS\n{'P': 5.4, 'x1': 0.199..., 'x3': 1.6}\n\n\n# Optimal tableau input:\n>>> {key: float(value) for key, value in Tableau(np.array([\n... [0, 0, 0.25, 0.25, 2],\n... [0, 1, 0.375, -0.125, 1],\n... [1, 0, -0.125, 0.375, 1]\n... ]), 2, 0).run_simplex().items()}\n{'P': 2.0, 'x1': 1.0, 'x2': 1.0}\n\n# Non-standard: >= constraints\nMax: 2x1 + 3x2 +  x3\nST:   x1 +  x2 +  x3 <= 40\n     2x1 +  x2 -  x3 >= 10\n         -  x2 +  x3 >= 10\n>>> {key: float(value) for key, value in Tableau(np.array([\n... [2, 0, 0, 0, -1, -1, 0, 0, 20],\n... [-2, -3, -1, 0, 0, 0, 0, 0, 0],\n... [1, 1, 1, 1, 0, 0, 0, 0, 40],\n... [2, 1, -1, 0, -1, 0, 1, 0, 10],\n... [0, -1, 1, 0, 0, -1, 0, 1, 10.]\n... ]), 3, 2).run_simplex().items()}\n{'P': 70.0, 'x1': 10.0, 'x2': 10.0, 'x3': 20.0}\n\n# Non standard: minimisation and equalities\nMin: x1 +  x2\nST: 2x1 +  x2 = 12\n    6x1 + 5x2 = 40\n>>> {key: float(value) for key, value in Tableau(np.array([\n... [8, 6, 0, 0, 52],\n... [1, 1, 0, 0, 0],\n... [2, 1, 1, 0, 12],\n... [6, 5, 0, 1, 40.],\n... ]), 2, 2).run_simplex().items()}\n{'P': 7.0, 'x1': 5.0, 'x2': 2.0}\n\n\n# Pivot on slack variables\nMax: 8x1 + 6x2\nST:   x1 + 3x2 <= 33\n     4x1 + 2x2 <= 48\n     2x1 + 4x2 <= 48\n      x1 +  x2 >= 10\n     x1        >= 2\n>>> {key: float(value) for key, value in Tableau(np.array([\n... [2, 1, 0, 0, 0, -1, -1, 0, 0, 12.0],\n... [-8, -6, 0, 0, 0, 0, 0, 0, 0, 0.0],\n... [1, 3, 1, 0, 0, 0, 0, 0, 0, 33.0],\n... [4, 2, 0, 1, 0, 0, 0, 0, 0, 60.0],\n... [2, 4, 0, 0, 1, 0, 0, 0, 0, 48.0],\n... [1, 1, 0, 0, 0, -1, 0, 1, 0, 10.0],\n... [1, 0, 0, 0, 0, 0, -1, 0, 1, 2.0]\n... ]), 2, 2).run_simplex().items()} # doctest: +ELLIPSIS\n{'P': 132.0, 'x1': 12.000... 'x2': 5.999...}\n\n>>> original_maxiter = Tableau.maxiter\n>>> Tableau.maxiter = 0\n>>> Tableau(np.array([[-1, -1, 0, 0, 0], [1, 3, 1, 0, 4],\n... [3, 1, 0, 1, 4.0]]), 2, 0).run_simplex()\nTraceback (most recent call last):\n...\nValueError: No convergence within 0 iterations; may be cycling/unbounded.\n>>> Tableau.maxiter = original_maxiter"
  [self] (let [--v (atom nil) col-idx (atom nil) msg (atom nil) row-idx (atom nil)] (try (doseq [__i1 (py-iter (range Tableau-maxiter))] (reset! --v __i1) (when (py-truthy (not (py-truthy (:objectives @self)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Tableau-interpret-tableau self)}))) (let [__d2 (vec (Tableau-find-pivot self))] (reset! row-idx (nth __d2 0)) (reset! col-idx (nth __d2 1))) (if (py-truthy (:stop-iter @self)) (do (swap! self (fn [__c] (py-assoc-in __c [:tableau] (Tableau-change-stage self))))) (do (swap! self (fn [__c] (py-assoc-in __c [:tableau] (Tableau-pivot self @row-idx @col-idx))))))) (reset! msg (str "No convergence within " (py-str Tableau-maxiter) " iterations; may be cycling/unbounded.")) (throw (ex-info (py-str @msg) {:type :ValueError})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Tableau-interpret-tableau "Given the final tableau, add the corresponding values of the basic\ndecision variables to the `output_dict`\n>>> {key: float(value) for key, value in Tableau(np.array([\n... [0,0,0.875,0.375,5],\n... [0,1,0.375,-0.125,1],\n... [1,0,-0.125,0.375,1]\n... ]),2, 0).interpret_tableau().items()}\n{'P': 5.0, 'x1': 1.0, 'x2': 1.0}"
  [& _args]
  ;; stub: original requires np,numpy-attr:nonzero; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:nonzero" {:type :UnsupportedOperation})))

(def Tableau-maxiter 100)

;; NOTE: Tableau __init__ stubbed (np,numpy-attr:all,numpy-attr:dtype,numpy-attr:shape)
;; NOTE: Tableau.find_pivot stubbed (np,numpy-attr:argmax)
;; NOTE: Tableau.pivot stubbed (np,numpy-attr:copy)
;; NOTE: Tableau.change_stage stubbed (np)
;; NOTE: Tableau.interpret_tableau stubbed (np,numpy-attr:nonzero)
