(ns clojure-algorithms.maths.numerical-analysis.bisection
  "Finds where a function becomes 0 in [a, b] using the bisection method.")

(defn bisection
  "Find where function becomes 0 in [a, b] using Bolzano's theorem.

  Returns a if a is a root, b if b is a root, otherwise bisects until the
  interval midpoint is stable within 1e-7. Throws ex-info with
  :type :ValueError when no root can be found in the given interval."
  [function a b]
  (if (zero? (function a))
    a
    (if (zero? (function b))
      b
      (do
        (when (pos? (* (function a) (function b)))
          (throw (ex-info "could not find root in given interval."
                          {:type :ValueError})))
        (loop [start a
               end b
               mid (+ a (/ (- b a) 2.0))]
          (if (<= (Math/abs (- (double start) (double mid))) 1e-7)
            mid
            (if (zero? (function mid))
              mid
              (let [[start end] (if (neg? (* (function mid) (function start)))
                                  [start mid]
                                  [mid end])]
                (recur start end (+ start (/ (- end start) 2.0)))))))))))

(defn f
  "Sample function f(x) = x^3 - 2x - 5."
  [x]
  (- (* x x x) (* 2 x) 5))
