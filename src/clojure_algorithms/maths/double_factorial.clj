(ns clojure-algorithms.maths.double-factorial
  "Double factorial. Theory: https://en.wikipedia.org/wiki/Double_factorial")

(defn- check-double-factorial-arg
  "Validates the argument for the double factorial functions."
  [fname n]
  (when-not (integer? n)
    (throw (ex-info (str fname "() only accepts integral values")
                    {:type :ValueError})))
  (when (neg? n)
    (throw (ex-info (str fname "() not defined for negative values")
                    {:type :ValueError}))))

(defn double-factorial-recursive
  "Compute double factorial using a recursive method.
Recursion can be costly for large numbers."
  [n]
  (check-double-factorial-arg "double_factorial_recursive" n)
  (if (<= n 1)
    1
    (*' n (double-factorial-recursive (- n 2)))))

(defn double-factorial-iterative
  "Compute double factorial using an iterative method."
  [num]
  (check-double-factorial-arg "double_factorial_iterative" num)
  (reduce *' 1 (range num 0 -2)))
