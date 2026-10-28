(ns clojure-algorithms.neural-network.two-hidden-layers-neural-network
  "Feedforward neural network with two hidden layers (4 and 3 units).

  Trained with backpropagation and a sigmoid nonlinearity for binary
  classification. Matrices are vectors of row vectors.
  See http://neuralnetworksanddeeplearning.com/chap2.html")

(defn sigmoid-value
  "Sigmoid of a single number."
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- (double x))))))

(defn sigmoid
  "Applies sigmoid element-wise to a scalar, vector, or matrix."
  [x]
  (cond
    (number? x) (sigmoid-value x)
    (and (sequential? x) (sequential? (first x))) (mapv sigmoid x)
    :else (mapv sigmoid-value x)))

(defn sigmoid-derivative
  "Applies the sigmoid derivative v * (1 - v) element-wise."
  [x]
  (cond
    (number? x) (* x (- 1 x))
    (and (sequential? x) (sequential? (first x))) (mapv sigmoid-derivative x)
    :else (mapv #(* % (- 1 %)) x)))

(defn- transpose
  "Transposes a matrix (vector of row vectors)."
  [m]
  (apply mapv vector m))

(defn- matmul
  "Multiplies matrices `a` (m x k) and `b` (k x n)."
  [a b]
  (let [bt (transpose b)]
    (mapv (fn [row] (mapv (fn [col] (reduce + (map * row col))) bt)) a)))

(defn- mat-add
  "Element-wise addition of two matrices."
  [a b]
  (mapv (fn [ra rb] (mapv + ra rb)) a b))

(defn- mat-sub
  "Element-wise subtraction of two matrices."
  [a b]
  (mapv (fn [ra rb] (mapv - ra rb)) a b))

(defn- mat-mul
  "Element-wise multiplication of two matrices."
  [a b]
  (mapv (fn [ra rb] (mapv * ra rb)) a b))

(defn- mat-scale
  "Scales every entry of matrix `m` by `s`."
  [m s]
  (mapv (fn [row] (mapv #(* s %) row)) m))

(defn make-network
  "Builds a network state map from `input-array` (n x k matrix) and
  `output-array` (n x 1 matrix) with random weights in [0, 1).
  Pass `rng` (java.util.Random) for reproducibility."
  ([input-array output-array] (make-network input-array output-array (java.util.Random.)))
  ([input-array output-array rng]
   {:input-array input-array
    :output-array output-array
    :w1 (vec (for [_ (range (count (first input-array)))]
               (vec (for [_ (range 4)] (.nextDouble rng)))))
    :w2 (vec (for [_ (range 4)]
               (vec (for [_ (range 3)] (.nextDouble rng)))))
    :w3 (vec (for [_ (range 3)]
               (vec (for [_ (range 1)] (.nextDouble rng)))))
    :predicted-output (vec (repeat (count output-array) [0.0]))}))

(defn feedforward
  "Runs one forward pass. Returns [updated-state output-matrix]."
  [{:keys [input-array w1 w2 w3] :as net}]
  (let [l1 (sigmoid (matmul input-array w1))
        l2 (sigmoid (matmul l1 w2))
        out (sigmoid (matmul l2 w3))]
    [(assoc net :l1 l1 :l2 l2 :predicted-output out) out]))

(defn back-propagation
  "Updates weights from the last forward pass. Returns updated state."
  [{:keys [output-array predicted-output l1 l2 w2 w3] :as net}]
  (let [err (mat-scale (mat-mul (mat-sub output-array predicted-output)
                                (sigmoid-derivative predicted-output))
                       2)
        d3 (matmul (transpose l2) err)
        e2 (mat-mul (matmul err (transpose w3)) (sigmoid-derivative l2))
        d2 (matmul (transpose l1) e2)
        e1 (mat-mul (matmul e2 (transpose w2)) (sigmoid-derivative l1))
        d1 (matmul (transpose (:input-array net)) e1)]
    (assoc net
           :w1 (mat-add (:w1 net) d1)
           :w2 (mat-add (:w2 net) d2)
           :w3 (mat-add (:w3 net) d3))))

(defn train
  "Trains for `iterations`, printing mean-square loss when `give-loss?`.
  Returns the final state."
  [net iterations give-loss?]
  (loop [n net
         i 1]
    (if (> i iterations)
      n
      (let [[n2 out] (feedforward n)
            n3 (back-propagation n2)]
        (when give-loss?
          (let [[_ out2] (feedforward n3)
                diffs (map - (flatten (:output-array n)) (flatten out2))
                loss (/ (reduce + (map #(* % %) diffs)) (count diffs))]
            (println (str "Iteration " i " Loss: " loss))))
        (recur n3 (inc i))))))

(defn predict
  "Predicts 0/1 for a single input row (threshold 0.6 on the output)."
  [{:keys [w1 w2 w3]} input-row]
  (let [l1 (sigmoid (matmul [input-row] w1))
        l2 (sigmoid (matmul l1 w2))
        out (sigmoid (matmul l2 w3))]
    (if (> (ffirst out) 0.6) 1 0)))

(defn example
  "Trains on the 3-bit truth table from the Python example and
  predicts the class of [1 1 1]. Returns 0 or 1."
  []
  (let [test-input [[0 0 0] [0 0 1] [0 1 0] [0 1 1]
                    [1 0 0] [1 0 1] [1 1 0] [1 1 1]]
        output [[0] [1] [1] [0] [1] [0] [0] [1]]
        net (make-network test-input output)]
    (predict (train net 10 false) [1 1 1])))
