(ns clojure-algorithms.linear-programming.simplex
  "Python implementation of the simplex algorithm for solving linear programs in\ntabular form with\n- `>=`, `<=`, and `=` constraints and\n- each variable `x1, x2, ...>= 0`.\n\nSee https://gist.github.com/imengus/f9619a568f7da5bc74eaf20169a24d98 for how to\nconvert linear programs to simplex tableaus, and the steps taken in the simplex\nalgorithm.\n\nResources:\nhttps://en.wikipedia.org/wiki/Simplex_algorithm\nhttps://tinyurl.com/simplex4beginners"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare make-Tableau Tableau-generate-col-titles Tableau-find-pivot Tableau-pivot Tableau-change-stage Tableau-run-simplex Tableau-interpret-tableau Tableau-maxiter)

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
