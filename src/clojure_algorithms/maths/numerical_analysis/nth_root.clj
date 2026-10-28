(ns clojure-algorithms.maths.numerical-analysis.nth-root
  "Approximate the nth root of a real number using Newton's Method.

The nth root of a real number R can be computed with Newton's method,
which starts with an initial guess x_0 and then iterates using the
recurrence relation:

x_{k + 1} = x_k - ((x_k)**n - R)/(n*(x_k)**(n-1))

The recurrence relation can be rewritten for computational efficiency:

x_{k + 1} = (n-1)/n*x_k + R/(n*(x_k)**(n-1))

Given a tolerance TOL, a stopping criterion can be set as:

abs(x_{k + 1} - x_k) < TOL

References:
- https://en.wikipedia.org/wiki/Nth_root#Using_Newton's_method
- Sauer, T. (2011): Numerical analysis.
    USA. Addison-Wesley Publishing Company.")

(defn- type-name
  [v]
  (cond (string? v) "str"
        (integer? v) "int"
        (float? v) "float"
        (boolean? v) "bool"
        (nil? v) "NoneType"
        :else (.getSimpleName (class v))))

(defn nth-root
  "Approximate the nth root of radicand for the given index.

  tolerance establishes the stopping criterion (default 0.0001).
  Throws ex-info with :type :TypeError for non-numeric radicand, non-integer
  index, or non-numeric tolerance, and with :type :ValueError for a
  non-positive index, a non-positive tolerance, or a negative radicand with
  an even index."
  ([radicand index] (nth-root radicand index 0.0001))
  ([radicand index tolerance]
   (when-not (or (integer? radicand) (float? radicand))
     (throw (ex-info (str "radicand must be a real number, not a "
                          (type-name radicand))
                     {:type :TypeError})))
   (when-not (integer? index)
     (throw (ex-info (str "index must be an integer, not a "
                          (type-name index))
                     {:type :TypeError})))
   (when (<= index 0)
     (throw (ex-info (str "index must be a positive integer, " index " <= 0")
                     {:type :ValueError})))
   (when-not (or (integer? tolerance) (float? tolerance))
     (throw (ex-info (str "tolerance must be a real number, not "
                          (type-name tolerance))
                     {:type :TypeError})))
   (when (<= tolerance 0)
     (throw (ex-info (str "tolerance must be a positive real number, "
                          tolerance " <= 0")
                     {:type :ValueError})))
   (when (and (neg? radicand) (even? index))
     (throw (ex-info "math domain error, radicand must be nonnegative for even index"
                     {:type :ValueError})))
   (if (zero? radicand)
     0.0
     (let [tol (double tolerance)]
       (loop [new-approx (double radicand)
              old-approx (+ (double radicand) tol 0.1)]
         (if (< (Math/abs (- old-approx new-approx)) tol)
           new-approx
           (let [first-summand (* (/ (double (dec index)) (double index))
                                  new-approx)
                 second-summand (/ (double radicand)
                                   (* (double index)
                                      (Math/pow new-approx (dec index))))]
             (recur (+ first-summand second-summand) new-approx))))))))
