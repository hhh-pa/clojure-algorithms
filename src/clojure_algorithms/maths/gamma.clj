(ns clojure-algorithms.maths.gamma
  "Gamma function is a very useful tool in math and physics.
  It helps calculating complex integral in a convenient way.
  for more info: https://en.wikipedia.org/wiki/Gamma_function
  In mathematics, the gamma function is one commonly
  used extension of the factorial function to complex numbers.
  The gamma function is defined for all complex numbers except
  the non-positive integers")

(def ^:private lanczos-coefficients
  "Lanczos approximation coefficients (g=7)."
  [0.9999999999998099 676.5203681218851 -1259.1392167224028
   771.32342877765313 -176.61502916214059 12.507343278686905
   -0.13857109526572012 9.984369578019572e-6 1.5056327351493116e-7])

(defn- lanczos-gamma
  "Gamma function for positive `z` via the Lanczos approximation
  (relative error ~1e-10)."
  [z]
  (if (< z 0.5)
    (/ Math/PI (* (Math/sin (* Math/PI z)) (lanczos-gamma (- 1 z))))
    (let [z (dec z)
          x (+ (first lanczos-coefficients)
               (reduce + (map-indexed (fn [i c] (/ c (+ z i 1)))
                                      (rest lanczos-coefficients))))
          t (+ z 7.5)]
      (* (Math/sqrt (* 2 Math/PI))
         (Math/pow t (+ z 0.5))
         (Math/exp (- t))
         x))))

(defn gamma-iterative
  "Calculates the value of the Gamma function of `num` for positive `num`.
  Upstream uses scipy numerical quadrature; this port uses the Lanczos
  approximation instead since scipy is unavailable in stdlib-only Clojure."
  [num]
  (when (<= num 0)
    (throw (ex-info "math domain error" {:type :ValueError})))
  (lanczos-gamma (double num)))

(defn integrand
  "Integrand x^(z-1) * e^(-x) of the Gamma function integral definition."
  [x z]
  (* (Math/pow x (- z 1)) (Math/exp (- x))))

(defn gamma-recursive
  "Calculates the value of the Gamma function of `num` using recursion,
  where `num` is either a positive integer or half-integer."
  [num]
  (cond
    (<= num 0)
    (throw (ex-info "math domain error" {:type :ValueError}))
    (> num 171.5)
    (throw (ex-info "math range error" {:type :OverflowError}))
    (not (or (zero? (- num (long num))) (== (- num (long num)) 0.5)))
    (throw (ex-info "num must be an integer or a half-integer"
                    {:type :NotImplementedError}))
    (== num 0.5) (Math/sqrt Math/PI)
    (== num 1) 1.0
    :else (* (dec num) (gamma-recursive (dec num)))))
