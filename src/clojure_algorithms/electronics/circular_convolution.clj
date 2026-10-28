(ns clojure-algorithms.electronics.circular-convolution
  "Circular (cyclic) convolution of two discrete signals.

  Pure-Clojure port of the matrix method; the Python original uses numpy
  only for the final matrix-vector product, done here with reduce.
  Ported from TheAlgorithms/Python electronics/circular_convolution.py.")

(defn- rotate-right
  "Rotate vector `v` right by `n` positions."
  [v n]
  (let [len (count v)
        n (mod n len)]
    (vec (concat (subvec v (- len n)) (subvec v 0 (- len n))))))

(defn circular-convolution
  "Circular convolution of `first-signal` and `second-signal`.
  Returns values rounded to 2 decimals."
  [first-signal second-signal]
  (let [n (max (count first-signal) (count second-signal))
        pad (fn [s] (vec (concat s (repeat (- n (count s)) 0))))
        x (pad (vec first-signal))
        h (pad (vec second-signal))
        matrix (mapv #(rotate-right h %) (range n))]
    (mapv (fn [j]
            (/ (Math/round (* 100.0 (reduce + (map * (mapv #(nth % j) matrix)
                                                   x))))
               100.0))
          (range n))))
