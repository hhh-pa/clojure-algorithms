(ns clojure-algorithms.maths.numerical-analysis.square-root
  "Square root approximated using Newton's method.
https://en.wikipedia.org/wiki/Newton%27s_method")

(defn fx
  "Evaluate x^2 - a."
  [x a]
  (- (* x x) a))

(defn fx-derivative
  "Evaluate the derivative 2x."
  [x]
  (* 2 x))

(defn get-initial-point
  "Return a starting point above a by repeated squaring from 2.0."
  [a]
  (loop [start 2.0]
    (if (<= start a)
      (recur (* start start))
      start)))

(defn square-root-iterative
  "Approximate the square root of a with Newton's method.

  max-iter bounds the iterations (default 9999), tolerance the stopping
  criterion (default 1e-14). Throws ex-info with :type :ValueError for
  negative a."
  ([a] (square-root-iterative a 9999 1e-14))
  ([a max-iter] (square-root-iterative a max-iter 1e-14))
  ([a max-iter tolerance]
   (when (neg? a)
     (throw (ex-info "math domain error" {:type :ValueError})))
   (loop [value (get-initial-point a)
          remaining max-iter]
     (if (zero? remaining)
       value
       (let [next-value (- value (/ (fx value a) (fx-derivative value)))]
         (if (< (Math/abs (- value next-value)) tolerance)
           next-value
           (recur next-value (dec remaining))))))))
