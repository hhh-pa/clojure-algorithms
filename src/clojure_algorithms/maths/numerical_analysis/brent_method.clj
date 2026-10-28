(ns clojure-algorithms.maths.numerical-analysis.brent-method
  "Root finding with Brent's method, combining bisection, secant, and inverse
quadratic interpolation.")

(defn brent-method
  "Find the root of func in the interval [left, right] using Brent's method.

  tol is the tolerance for convergence (default 1e-8) and max-iter the maximum
  number of iterations (default 100). Throws ex-info with :type :ValueError
  when func(left) and func(right) do not have opposite signs."
  ([func left right] (brent-method func left right 1e-8 100))
  ([func left right tol] (brent-method func left right tol 100))
  ([func left right tol max-iter]
   (let [f (fn [x] (double (func x)))
         left (double left)
         right (double right)
         fl (f left)
         fr (f right)]
     (when (>= (* fl fr) 0)
       (throw (ex-info "func(left) and func(right) must have opposite signs"
                       {:type :ValueError})))
     (let [[left right fl fr] (if (< (Math/abs fl) (Math/abs fr))
                                [right left fr fl]
                                [left right fl fr])]
       (loop [iteration 0
              left left right right fl fl fr fr
              c left fc fl d (- right left)]
         (if (>= iteration max-iter)
           right
           (if (zero? fr)
             right
             (let [s (if (or (= fc fl) (= fc fr))
                       (- right (/ (* fr (- right left)) (- fr fl)))
                       (+ (/ (* left fr fc) (* (- fl fr) (- fl fc)))
                          (/ (* right fl fc) (* (- fr fl) (- fr fc)))
                          (/ (* c fl fr) (* (- fc fl) (- fc fr)))))
                   m (/ (+ (* 3 left) right) 4)
                   bisect? (or (if (> right left)
                                 (not (and (< m s) (< s right)))
                                 (not (and (< right s) (< s m))))
                               (and (> iteration 1)
                                    (>= (Math/abs (- s right))
                                        (/ (Math/abs (- right c)) 2)))
                               (and (<= iteration 1)
                                    (>= (Math/abs (- s right))
                                        (/ (Math/abs (- c d)) 2)))
                               (and (> iteration 1) (< (Math/abs (- right c)) tol))
                               (and (<= iteration 1) (< (Math/abs (- c d)) tol)))
                   s (if bisect? (/ (+ left right) 2) s)
                   d (if bisect? (- right left) d)
                   fs (f s)
                   d c
                   c right
                   fc fr
                   [left right fl fr] (if (neg? (* fl fs))
                                        [left s fl fs]
                                        [s right fs fr])
                   [left right fl fr] (if (< (Math/abs fl) (Math/abs fr))
                                        [right left fr fl]
                                        [left right fl fr])]
               (if (< (Math/abs (- right left)) tol)
                 right
                 (recur (inc iteration)
                        left right fl fr c fc d))))))))))
