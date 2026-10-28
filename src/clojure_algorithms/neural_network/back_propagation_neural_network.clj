(ns clojure-algorithms.neural-network.back-propagation-neural-network
  "Back-propagation neural network (BPNN) framework.

  Stacked dense layers trained with forward propagation and
  gradient backpropagation on mean-square error. Vectors and
  row-major matrices are plain Clojure vectors. (The Python
  original plots the loss curve with matplotlib; this port keeps
  the loss history in the network map instead.)")

(defn sigmoid-value
  "Sigmoid of a single number."
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- (double x))))))

(defn sigmoid
  "Applies sigmoid element-wise to vector `v`."
  [v]
  (mapv sigmoid-value v))

(defn linear
  "Identity activation over vector `v`."
  [v]
  (vec v))

(defn make-dense-layer
  "Builds an uninitialized dense layer with `units` neurons.
  Options: :learning-rate (0.3), :activation (sigmoid),
  :input-layer? (false)."
  [units & {:keys [learning-rate activation input-layer?]
            :or {learning-rate 0.3 activation sigmoid input-layer? false}}]
  {:units units
   :weights nil
   :bias nil
   :activation activation
   :learn-rate learning-rate
   :input-layer? input-layer?})

(defn init-layer
  "Initializes `layer` weights/biases for `back-units` inputs using
  gaussian samples scaled by 0.5. `rng` is a java.util.Random."
  [layer back-units rng]
  (assoc layer
         :weights (vec (for [_ (range (:units layer))]
                         (vec (for [_ (range back-units)] (* 0.5 (.nextGaussian rng))))))
         :bias (vec (for [_ (range (:units layer))] (* 0.5 (.nextGaussian rng))))))

(defn- matvec
  "Multiplies matrix `m` (rows x cols) by column vector `v`."
  [m v]
  (mapv (fn [row] (reduce + (map * row v))) m))

(defn layer-forward
  "Runs `x` through `layer`. Returns [updated-layer output]."
  [{:keys [input-layer? weights bias activation] :as layer} x]
  (if input-layer?
    [(assoc layer :xdata x :output x) x]
    (let [z (mapv - (matvec weights x) bias)
          out (activation z)]
      [(assoc layer :xdata x :wx-plus-b z :output out) out])))

(defn- outer
  "Outer product of vectors `a` and `b` as a matrix."
  [a b]
  (mapv (fn [ai] (mapv #(* ai %) b)) a))

(defn- transpose
  "Transposes a matrix (vector of row vectors)."
  [m]
  (apply mapv vector m))

(defn layer-backward
  "Backpropagates `gradient` through `layer`, updating weights/bias.
  Returns [updated-layer next-gradient]."
  [{:keys [weights bias learn-rate output xdata activation] :as layer} gradient]
  (let [g (if (= activation sigmoid)
            (mapv (fn [gi oi] (* gi oi (- 1 oi))) gradient output)
            gradient)
        grad-w (outer g xdata)
        grad-b (mapv - g)
        next-grad (matvec (transpose weights) g)]
    [(assoc layer
            :weights (mapv (fn [rw rg] (mapv (fn [w dw] (- w (* learn-rate dw))) rw rg))
                           weights grad-w)
            :bias (mapv (fn [b db] (- b (* learn-rate db))) bias grad-b))
     next-grad]))

(defn make-bpnn
  "Builds an empty back-propagation network."
  []
  {:layers [] :train-mse []})

(defn add-layer
  "Appends `layer` to the network."
  [net layer]
  (update net :layers conj layer))

(defn build-network
  "Marks the first layer as input and initializes the rest.
  `rng` is a java.util.Random (fresh one by default)."
  ([net] (build-network net (java.util.Random.)))
  ([net rng]
   (assoc net :layers
          (vec (map-indexed
                (fn [i layer]
                  (if (zero? i)
                    (assoc layer :input-layer? true)
                    (init-layer layer (:units (nth (:layers net) (dec i))) rng)))
                (:layers net))))))

(defn cal-loss
  "Squared-error loss and gradient between target `y` and output `yh`.
  Returns [loss gradient]."
  [y yh]
  (let [diff (mapv - yh y)]
    [(reduce + (map #(* % %) (map - y yh)))
     (mapv #(* 2 %) diff)]))

(defn train
  "Trains on row-matrices `xdata`/`ydata` for `rounds` epochs or until
  MSE drops below `accuracy`. Returns [updated-net final-mse-or-nil]."
  [net xdata ydata rounds accuracy]
  (loop [n net
         r 0]
    (if (>= r rounds)
      [n nil]
      (let [[layers' total] (reduce (fn [[layers acc] [xrow yrow]]
                                      (let [[fwd-layers out]
                                            (reduce (fn [[ls x] l]
                                                      (let [[l' o] (layer-forward l x)]
                                                        [(conj ls l') o]))
                                                    [[] (vec xrow)]
                                                    layers)
                                            [loss grad] (cal-loss (vec yrow) out)
                                            rev (reduce (fn [[ls g] l]
                                                          (let [[l' g'] (layer-backward l g)]
                                                            [(conj ls l') g']))
                                                        [[] grad]
                                                        (rseq (subvec fwd-layers 1)))
                                            new-layers (into [(first fwd-layers)] (reverse (first rev)))]
                                        [new-layers (+ acc loss)]))
                                    [(:layers n) 0.0]
                                    (map vector xdata ydata))
            mse (/ total (count xdata))
            n' (assoc n :layers layers' :train-mse (conj (:train-mse n) mse))]
        (if (< mse accuracy)
          [n' mse]
          (recur n' (inc r)))))))

(defn example
  "Trains a 10-20-30-2 network on random data for 100 rounds.
  Returns [trained-net final-mse-or-nil]."
  []
  (let [rng (java.util.Random. 42)
        x (vec (for [_ (range 10)]
                 (vec (for [_ (range 10)] (.nextGaussian rng)))))
        y [[0.8 0.4] [0.4 0.3] [0.34 0.45] [0.67 0.32] [0.88 0.67]
           [0.78 0.77] [0.55 0.66] [0.55 0.43] [0.54 0.1] [0.1 0.5]]
        net (reduce add-layer (make-bpnn) (map make-dense-layer [10 20 30 2]))]
    (train (build-network net rng) x y 100 0.01)))
