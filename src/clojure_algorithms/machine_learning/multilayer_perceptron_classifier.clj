(ns clojure-algorithms.machine-learning.multilayer-perceptron-classifier
  "Multi-layer perceptron classifier with forward/backward passes.

  Matrices are vectors of row vectors. The MLP's intermediate forward-pass
  cache lives in an atom (matching the upstream instance attribute), while
  training returns an updated model map.")

(def rng
  "Shared random source (seeded, like the upstream module-level default_rng)."
  (java.util.Random. 42))

;; ---------------------------------------------------------------------------
;; matrix helpers
;; ---------------------------------------------------------------------------

(defn- matmul
  [a b]
  (let [bt (apply mapv vector b)]
    (mapv (fn [row] (mapv (fn [col] (reduce + (map * row col))) bt)) a)))

(defn- transpose
  [matrix]
  (apply mapv vector matrix))

(defn- elementwise
  [f a b]
  (mapv (fn [ra rb] (mapv f ra rb)) a b))

(defn- deep-map
  [f x]
  (if (sequential? x) (mapv #(deep-map f %) x) (f x)))

(defn- argmax-first
  [xs]
  (first (keep-indexed (fn [i v] (when (= v (apply max xs)) i)) xs)))

;; ---------------------------------------------------------------------------
;; Dataloader
;; ---------------------------------------------------------------------------

(defn make-Dataloader
  "Loader over feature rows X with integer labels y."
  [features labels]
  {:kind :dataloader
   :X (mapv vec features)
   :y (vec labels)
   :class-weights {0 1.0 1 1.0}})

(defn Dataloader-get-train-test-data
  "Manual split: first three rows train, last row test.
  Returns [train-data train-labels test-data test-labels] with labels as
  single-element vectors."
  [loader]
  (let [{:keys [X y]} loader]
    [(vec (take 3 X))
     (mapv vector (take 3 y))
     [(nth X 3)]
     [(vector (nth y 3))]]))

(defn Dataloader-shuffle-data
  "Return paired-data unchanged (the upstream shuffling is a no-op stub)."
  [_loader paired-data]
  paired-data)

(defmulti poly-get-inout-dim
  "Input dimension and output dimension."
  (fn [x] (:kind x)))

(defmethod poly-get-inout-dim :dataloader
  [loader]
  (let [[train-data train-labels _ _] (Dataloader-get-train-test-data loader)]
    [(count (first train-data)) (count train-labels)]))

(def Dataloader-get-inout-dim poly-get-inout-dim)

(defn- as-label
  [label]
  (if (sequential? label) (first label) label))

(defn Dataloader-one-hot-encode
  "0.99 at each label index, else 0. Labels may be ints or 1-vectors."
  [labels num-classes]
  (mapv (fn [label]
          (assoc (vec (repeat num-classes 0.0)) (as-label label) 0.99))
        labels))

;; ---------------------------------------------------------------------------
;; MLP
;; ---------------------------------------------------------------------------

(defn make-MLP
  "MLP over dataloader (may be nil until training) with epoch count,
  learning rate, decay gamma and hidden dimension."
  ([dataloader epoch learning-rate]
   (make-MLP dataloader epoch learning-rate 1.0 2))
  ([dataloader epoch learning-rate gamma hidden-dim]
   {:kind :mlp
    :learning-rate learning-rate
    :gamma gamma
    :epoch epoch
    :hidden-dim hidden-dim
    :train-loss []
    :train-accuracy []
    :test-loss []
    :test-accuracy []
    :dataloader dataloader
    :inter-variable (atom {})
    :weights1-list []}))

(defmethod poly-get-inout-dim :mlp
  [mlp]
  (poly-get-inout-dim (:dataloader mlp)))

(def MLP-get-inout-dim poly-get-inout-dim)

(defn MLP-initialize
  "He-initialized [w1 w2] weight matrices."
  [mlp]
  (let [[in-dim out-dim] (poly-get-inout-dim mlp)
        hidden (:hidden-dim mlp)
        randn (fn [] (.nextGaussian ^java.util.Random rng))]
    [(mapv (fn [_] (mapv (fn [_] (* (randn) (Math/sqrt (/ 2.0 in-dim))))
                         (range hidden)))
           (range (inc in-dim)))
     (mapv (fn [_] (mapv (fn [_] (* (randn) (Math/sqrt (/ 2.0 hidden))))
                         (range out-dim)))
           (range hidden))]))

(defn MLP-relu
  "Elementwise max(0, x)."
  [_mlp input-array]
  (deep-map #(max 0 %) input-array))

(defn MLP-relu-derivative
  "Elementwise 1.0 where positive, else 0.0."
  [_mlp input-array]
  (deep-map #(if (pos? %) 1.0 0.0) input-array))

(defn MLP-forward
  "Forward pass; caches intermediates unless no-gradient is true.
  Returns the output matrix."
  ([mlp input-data w1 w2] (MLP-forward mlp input-data w1 w2 false))
  ([mlp input-data w1 w2 no-gradient]
   (let [z1 (matmul input-data w1)
         a1 (MLP-relu mlp z1)
         z2 (matmul a1 w2)
         a2 z2]
     (when-not no-gradient
       (reset! (:inter-variable mlp) {:z1 z1 :a1 a1 :z2 z2 :a2 a2}))
     a2)))

(defn MLP-back-prop
  "Gradients [grad-w1 grad-w2] from the cached forward pass."
  [mlp input-data true-labels w2]
  (let [{:keys [a1 z1 a2]} @(:inter-variable mlp)
        batch-size (count input-data)
        delta-k (elementwise - a2 true-labels)
        delta-j (elementwise * (matmul delta-k (transpose w2))
                             (MLP-relu-derivative mlp z1))
        scale-down (fn [rows] (mapv (fn [row] (mapv #(/ % batch-size) row))
                                    rows))
        grad-w2 (scale-down (matmul (transpose a1) delta-k))
        grad-w1 (scale-down (matmul (transpose input-data) delta-j))]
    [grad-w1 grad-w2]))

(defn MLP-update-weights
  "Gradient step on both matrices; returns [w1 w2]."
  [_mlp w1 w2 grad-w1 grad-w2 learning-rate]
  [(elementwise (fn [w g] (- w (* learning-rate g))) w1 grad-w1)
   (elementwise (fn [w g] (- w (* learning-rate g))) w2 grad-w2)])

(defn MLP-update-learning-rate
  "Decay the learning rate by gamma."
  [mlp learning-rate]
  (* learning-rate (:gamma mlp)))

(defn MLP-accuracy
  "Fraction of rows where predicted and true argmax agree."
  [label y-hat]
  (/ (double (count (filter true? (map #(= (argmax-first %1)
                                           (argmax-first %2))
                                       label y-hat))))
     (count label)))

(defn MLP-loss
  "Half mean squared error between outputs and labels."
  [output label]
  (/ (double (reduce + (map #(let [d (- (double %1) (double %2))] (* d d))
                            (mapcat identity output)
                            (mapcat identity label))))
     (* 2 (count label))))

(defn MLP-get-acc-loss
  "Recorded [test-accuracy test-loss]."
  [mlp]
  [(:test-accuracy mlp) (:test-loss mlp)])

(defn MLP-train
  "Train with mini-batches of 1, tracking test accuracy/loss per epoch.
  Prints the mean test accuracy; returns the updated model."
  [mlp]
  (let [[train-data train-labels test-data test-labels]
        (Dataloader-get-train-test-data (:dataloader mlp))
        with-bias (fn [rows] (mapv #(conj (vec %) 1.0) rows))
        train-data (with-bias train-data)
        test-data (with-bias test-data)
        [_ total-labels] (poly-get-inout-dim (:dataloader mlp))
        train-labels (Dataloader-one-hot-encode train-labels total-labels)
        test-labels (Dataloader-one-hot-encode test-labels total-labels)
        [w1 w2] (MLP-initialize mlp)
        batch-size 1]
    (loop [epoch 0
           w1 w1
           w2 w2
           learning-rate (:learning-rate mlp)
           accuracies []
           losses []]
      (if (>= epoch (:epoch mlp))
        (do (println (str "Test accuracy: "
                          (/ (double (reduce + accuracies))
                             (count accuracies))))
            (assoc mlp :test-accuracy accuracies :test-loss losses))
        (let [[w1 w2] (loop [k 0
                             w1 w1
                             w2 w2]
                        (if (>= k (count train-data))
                          [w1 w2]
                          (let [batch-imgs (subvec train-data k
                                                   (min (+ k batch-size)
                                                        (count train-data)))
                                batch-labels (subvec train-labels k
                                                     (min (+ k batch-size)
                                                          (count train-labels)))]
                            (MLP-forward mlp batch-imgs w1 w2 false)
                            (let [[grad-w1 grad-w2] (MLP-back-prop
                                                     mlp batch-imgs
                                                     batch-labels w2)
                                  [w1 w2] (MLP-update-weights mlp w1 w2 grad-w1
                                                              grad-w2
                                                              learning-rate)]
                              (recur (+ k batch-size) w1 w2)))))
              test-output (MLP-forward mlp test-data w1 w2 true)]
          (recur (inc epoch) w1 w2
                 (MLP-update-learning-rate mlp learning-rate)
                 (conj accuracies (MLP-accuracy test-labels test-output))
                 (conj losses (MLP-loss test-output test-labels))))))))
