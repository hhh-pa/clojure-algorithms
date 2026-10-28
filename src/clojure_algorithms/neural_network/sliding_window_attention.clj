(ns clojure-algorithms.neural-network.sliding-window-attention
  "Sliding window attention for sequence modeling.

  Each position attends to a fixed-size window of neighbours:
  local frames are projected by a learned weight matrix and averaged.
  Tensors are nested vectors [batch [time [embed]]].
  See https://en.wikipedia.org/wiki/Attention_(machine_learning)")

(defn make-attention
  "Builds an attention module map with `embed-dim` and `window-size`.
  Weights are an embed-dim x embed-dim matrix of gaussian samples;
  pass `rng` (java.util.Random) for reproducibility."
  ([embed-dim window-size] (make-attention embed-dim window-size (java.util.Random.)))
  ([embed-dim window-size rng]
   {:embed-dim embed-dim
    :window-size window-size
    :attention-weights (vec (for [_ (range embed-dim)]
                              (vec (for [_ (range embed-dim)]
                                     (.nextGaussian rng)))))}))

(defn- matvec
  "Multiplies row-vector `v` by matrix `m` (v @ m)."
  [v m]
  (let [cols (apply mapv vector m)]
    (mapv (fn [col] (reduce + (map * v col))) cols)))

(defn- mean-vectors
  "Element-wise mean of a non-empty seq of equal-length vectors."
  [vecs]
  (let [n (count vecs)]
    (mapv #(/ % n) (reduce (fn [a b] (mapv + a b)) vecs))))

(defn forward
  "Forward pass over `input-tensor` ([batch [time [embed]]]).
  Returns an output tensor of the same shape."
  [{:keys [window-size attention-weights]} input-tensor]
  (let [seq-len (count (first input-tensor))
        half (quot window-size 2)]
    (mapv (fn [batch]
            (mapv (fn [i]
                    (let [start (max 0 (- i half))
                          end (min seq-len (+ i half 1))
                          window (subvec (vec batch) start end)
                          scored (mapv #(matvec % attention-weights) window)]
                      (mean-vectors scored)))
                  (range seq-len)))
          input-tensor)))
