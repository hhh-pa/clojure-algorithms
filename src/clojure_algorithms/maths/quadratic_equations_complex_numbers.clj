(ns clojure-algorithms.maths.quadratic-equations-complex-numbers
  "Port of maths/quadratic_equations_complex_numbers.py")

(defn quadratic-roots
  "Given the numerical coefficients a, b and c, calculate the roots of
  the quadratic equation ax^2 + bx + c. Returns a vector [root1 root2]
  where each root is a double for real roots or a [re im] pair of
  doubles for complex roots."
  [a b c]
  (when (zero? a)
    (throw (ex-info "Coefficient 'a' must not be zero." {:type :ValueError})))
  (let [a (double a)
        b (double b)
        c (double c)
        delta (- (* b b) (* 4.0 a c))]
    (if (neg? delta)
      (let [real (/ (- b) (* 2.0 a))
            imag (/ (Math/sqrt (- delta)) (* 2.0 a))]
        [[real imag] [real (- imag)]])
      (let [s (Math/sqrt delta)]
        [(/ (+ (- b) s) (* 2.0 a))
         (/ (- (- b) s) (* 2.0 a))]))))

(defn main
  "Print the solutions of 5x^2 + 6x + 1."
  []
  (let [[solution1 solution2] (quadratic-roots 5 6 1)]
    (println (str "The solutions are: " solution1 " and " solution2))))
