(ns clojure-algorithms.maths.numerical-analysis.bisection-2
  "Given a function on floating number f(x) and two floating numbers `a` and `b` such that
f(a) * f(b) < 0 and f(x) is continuous in [a, b].
Here f(x) represents algebraic or transcendental equation.
Find root of function in interval [a, b] (Or find a value of x such that f(x) is 0)

https://en.wikipedia.org/wiki/Bisection_method")

(defn equation
  "Sample equation f(x) = 10 - x^2."
  [x]
  (- 10 (* x x)))

(defn bisection
  "Find a root of equation in [a, b] by bisection.

  Throws ex-info with :type :ValueError when f(a) and f(b) do not have
  opposite signs."
  [a b]
  (when (>= (* (equation a) (equation b)) 0)
    (throw (ex-info "Wrong space!" {:type :ValueError})))
  (loop [a (double a)
         b (double b)
         c (double a)]
    (if (< (- b a) 0.01)
      c
      (let [c (/ (+ a b) 2)]
        (if (zero? (equation c))
          c
          (if (neg? (* (equation c) (equation a)))
            (recur a c c)
            (recur c b c)))))))
