(ns clojure-algorithms.maths.abs
  "Absolute Value.")

(defn abs-val
  "Find the absolute value of a number."
  [num]
  (if (neg? num) (- num) num))

(defn abs-min
  "Return the element of x with the smallest absolute value (first on ties)."
  [x]
  (when (empty? x)
    (throw (ex-info "abs_min() arg is an empty sequence" {:type :ValueError})))
  (reduce (fn [j i] (if (< (abs-val i) (abs-val j)) i j)) x))

(defn abs-max
  "Return the element of x with the largest absolute value (first on ties)."
  [x]
  (when (empty? x)
    (throw (ex-info "abs_max() arg is an empty sequence" {:type :ValueError})))
  (reduce (fn [j i] (if (> (abs-val i) (abs-val j)) i j)) x))

(defn abs-max-sort
  "Return the element of x with the largest absolute value via sorting."
  [x]
  (when (empty? x)
    (throw (ex-info "abs_max_sort() arg is an empty sequence" {:type :ValueError})))
  (last (sort-by abs-val x)))

(defn test-abs-val
  "Run inline assertions over the abs functions; returns nil."
  []
  (assert (= (abs-val 0) 0))
  (assert (= (abs-val 34) 34))
  (assert (= (abs-val -100000000000) 100000000000))
  (let [a [-3 -1 2 -11]]
    (assert (= (abs-max a) -11))
    (assert (= (abs-max-sort a) -11))
    (assert (= (abs-min a) -1)))
  nil)
