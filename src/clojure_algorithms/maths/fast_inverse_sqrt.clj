(ns clojure-algorithms.maths.fast-inverse-sqrt
  "Fast inverse square root (1/sqrt(x)) using the Quake III algorithm.
  Reference: https://en.wikipedia.org/wiki/Fast_inverse_square_root")

(defn fast-inverse-sqrt
  "Computes the fast inverse square root of the positive number `number`
  using the Quake III bit-manipulation algorithm."
  [number]
  (when (<= number 0)
    (throw (ex-info "Input must be a positive number." {:type :ValueError})))
  (let [i (Float/floatToIntBits (float number))
        i (- 0x5F3759DF (bit-shift-right i 1))
        y (Float/intBitsToFloat (int i))]
    (* y (- 1.5 (* 0.5 number y y)))))
