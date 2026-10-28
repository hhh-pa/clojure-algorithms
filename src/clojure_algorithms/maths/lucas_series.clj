(ns clojure-algorithms.maths.lucas-series
  "https://en.wikipedia.org/wiki/Lucas_number")

(defn recursive-lucas-number
  "Returns the nth lucas number, computed recursively."
  [n-th-number]
  (when-not (integer? n-th-number)
    (throw (ex-info "recursive_lucas_number accepts only integer arguments."
                    {:type :TypeError})))
  (cond
    (zero? n-th-number) 2
    (= n-th-number 1) 1
    :else (+' (recursive-lucas-number (dec n-th-number))
              (recursive-lucas-number (- n-th-number 2)))))

(defn dynamic-lucas-number
  "Returns the nth lucas number, computed iteratively."
  [n-th-number]
  (when-not (integer? n-th-number)
    (throw (ex-info "dynamic_lucas_number accepts only integer arguments."
                    {:type :TypeError})))
  (loop [a 2 b 1 i 0]
    (if (>= i n-th-number)
      a
      (recur b (+' a b) (inc i)))))
