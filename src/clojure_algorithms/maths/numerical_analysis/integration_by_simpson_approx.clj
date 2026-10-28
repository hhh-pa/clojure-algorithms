(ns clojure-algorithms.maths.numerical-analysis.integration-by-simpson-approx
  "Author : Syed Faizan ( 3rd Year IIIT Pune )
Github : faizan2700

Purpose : You have one function f(x) which takes float integer and returns
float you have to integrate the function in limits a to b.
The approximation proposed by Thomas Simpson in 1743 is one way to calculate
integration.

( read article : https://cp-algorithms.com/num_methods/simpson-integration.html )

simpson-integration takes function, lower_limit=a, upper_limit=b, precision and
returns the integration of function in given limit.")

(def N-STEPS 1000)

(defn f
  "Sample function f(x) = x^2."
  [x]
  (* x x))

(defn- round-to
  [value precision]
  (Double/parseDouble
   (.toPlainString (.setScale (BigDecimal. ^double (double value))
                              precision
                              java.math.RoundingMode/HALF_EVEN))))

(defn simpson-integration
  "Approximate the integral of function from a to b with Simpson's rule.

  precision is the number of decimals of the result (default 4).
  Throws AssertionError when the function is not callable, a or b is not a
  number, the function does not return a number, or precision is not a
  positive integer."
  ([function a b] (simpson-integration function a b 4))
  ([function a b precision]
   (assert (ifn? function)
           (str "the function(object) passed should be callable your input : "
                function))
   (assert (number? a) (str "a should be float or integer your input : " a))
   (assert (number? (function a))
           (str "the function should return integer or float return type of "
                "your function, " (type a)))
   (assert (number? b) (str "b should be float or integer your input : " b))
   (assert (and (integer? precision) (pos? precision))
           (str "precision should be positive integer your input : " precision))
   (let [h (/ (- (double b) (double a)) N-STEPS)
         fa (double (function a))
         fb (double (function b))
         result (reduce (fn [acc i]
                          (+ acc (* (double (function (+ (double a) (* h i))))
                                    (if (odd? i) 4 2))))
                        (+ fa fb)
                        (range 1 N-STEPS))]
     (round-to (* result (/ h 3)) precision))))
