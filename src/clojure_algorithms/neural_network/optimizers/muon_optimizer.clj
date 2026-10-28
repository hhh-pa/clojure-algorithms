(ns clojure-algorithms.neural-network.optimizers.muon-optimizer
  "Muon optimizer for hidden-layer weight matrices.

  Applies Newton-Schulz orthogonalization to gradients before
  a momentum update. Matrices are vectors of row vectors.
  See https://kellerjordan.github.io/posts/muon/")

(defn make-muon
  "Builds a Muon optimizer state map.
  Defaults: learning-rate 0.02, momentum 0.95, ns-steps 5."
  ([] (make-muon 0.02 0.95 5))
  ([learning-rate momentum] (make-muon learning-rate momentum 5))
  ([learning-rate momentum ns-steps]
   {:learning-rate (double learning-rate)
    :momentum (double momentum)
    :ns-steps ns-steps
    :velocity {}}))

(defn- transpose
  "Transposes a matrix (vector of row vectors)."
  [m]
  (apply mapv vector m))

(defn- matmul
  "Multiplies matrices `a` (m x k) and `b` (k x n)."
  [a b]
  (let [bt (transpose b)]
    (mapv (fn [row]
            (mapv (fn [col] (reduce + (map * row col))) bt))
          a)))

(defn- mat-scale
  "Scales every entry of `m` by `s`."
  [m s]
  (mapv (fn [row] (mapv #(* s %) row)) m))

(defn- mat-sub
  "Element-wise subtraction `a - b`."
  [a b]
  (mapv (fn [ra rb] (mapv - ra rb)) a b))

(defn newton-schulz-orthogonalize
  "Orthogonalizes `matrix` with `ns-steps` Newton-Schulz iterations."
  [matrix ns-steps]
  (let [transposed? (< (count matrix) (count (first matrix)))
        a0 (if transposed? (transpose matrix) matrix)
        aN (nth (iterate (fn [a]
                           (mat-sub (mat-scale a 1.5)
                                    (mat-scale (matmul a (matmul (transpose a) a)) 0.5)))
                         a0)
                ns-steps)]
    (if transposed? (transpose aN) aN)))

(defn update
  "Updates `params` matrix with `grads` matrix for group `param-id`.
  Returns [new-state new-params]."
  [state param-id params grads]
  (let [{:keys [learning-rate momentum ns-steps velocity]} state
        nrows (count params)
        ncols (count (first params))
        zeros (vec (repeat nrows (vec (repeat ncols 0.0))))
        vel (get velocity param-id zeros)
        ortho (newton-schulz-orthogonalize grads ns-steps)
        vel' (mapv (fn [rv ro] (mapv (fn [v o] (+ (* momentum v) o)) rv ro))
                   vel ortho)
        params' (mapv (fn [rp rv]
                        (mapv (fn [p v] (- p (* learning-rate v))) rp rv))
                      params vel')]
    [(assoc state :velocity (assoc velocity param-id vel')) params']))
