(ns clojure-algorithms.maths.numerical-analysis.nevilles-method
  "Python program to show how to interpolate and evaluate a polynomial
using Neville's method.
Neville's method evaluates a polynomial that passes through a
given set of x and corresponding y points for a particular x value (x0) using the
Newton polynomial form.
Reference:
    https://rpubs.com/aaronsc32/nevilles-method-polynomial-interpolation")

(defn neville-interpolate
  "Interpolate and evaluate a polynomial using Neville's method.

  x-points and y-points are the points the polynomial passes through, x0 is
  the value of x to evaluate the polynomial for. Returns a vector of the
  approximated value and the Neville iterations table."
  [x-points y-points x0]
  (when-not (number? x0)
    (throw (ex-info (str "unsupported operand type(s) for -: '"
                         (cond (string? x0) "str"
                               (boolean? x0) "bool"
                               (nil? x0) "NoneType"
                               :else (.getSimpleName (class x0)))
                         "' and 'int'")
                    {:type :TypeError})))
  (let [xs (vec x-points)
        ys (vec y-points)
        n (count xs)
        table (reduce (fn [q i] (assoc-in q [i 1] (nth ys i)))
                      (vec (repeat n (vec (repeat n 0))))
                      (range n))]
    (loop [i 2
           q table]
      (if (>= i n)
        [(get-in q [(dec n) (dec n)]) q]
        (recur (inc i)
               (loop [j i
                      q q]
                 (if (>= j n)
                   q
                   (recur (inc j)
                          (assoc-in q [j i]
                                    (/ (double (- (* (- x0 (nth xs (- (+ j 1) i)))
                                                     (get-in q [j (dec i)]))
                                                  (* (- x0 (nth xs j))
                                                     (get-in q [(dec j) (dec i)]))))
                                       (double (- (nth xs j)
                                                  (nth xs (- (+ j 1) i))))))))))))))
