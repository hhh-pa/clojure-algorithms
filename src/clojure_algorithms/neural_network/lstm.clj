(ns clojure-algorithms.neural-network.lstm
  "Long Short-Term Memory (LSTM) network for character sequences.

  Plain-Clojure port of the Python LSTM: one-hot inputs, forget/input/
  output gates with a cell candidate, softmax outputs, and full
  backpropagation through time. Column vectors and row-major matrices
  are plain vectors; the model is an immutable map threaded through
  `forward-pass`, `backward-pass`, and `train`.
  (Vocabulary order is sorted for determinism; the Python original
  inherits nondeterministic set order.)")

(defn- zeros
  "Zero vector of length `n`."
  [n]
  (vec (repeat n 0.0)))

(defn- transpose
  "Transposes a matrix."
  [m]
  (apply mapv vector m))

(defn- matvec
  "Multiplies matrix `m` by vector `v`."
  [m v]
  (mapv (fn [row] (reduce + (map * row v))) m))

(defn- outer
  "Outer product of vectors `a` and `b`."
  [a b]
  (mapv (fn [ai] (mapv #(* ai %) b)) a))

(defn- v+ [a b] (mapv + a b))
(defn- v- [a b] (mapv - a b))
(defn- emul [a b] (mapv * a b))
(defn- vscale [v s] (mapv #(* s %) v))
(defn- m+ [a b] (mapv (fn [ra rb] (mapv + ra rb)) a b))
(defn- m-scale [m s] (mapv (fn [r] (mapv #(* s %) r)) m))

(defn- clip-mat
  "Clips every entry of a vector or matrix into [-1, 1]."
  [x]
  (if (sequential? (first x))
    (mapv clip-mat x)
    (mapv #(max -1.0 (min 1.0 %)) x)))

(defn sigmoid-act
  "Sigmoid over vector `v`; with `deriv?` returns v .* (1 - v)."
  ([v] (sigmoid-act v false))
  ([v deriv?]
   (if deriv?
     (emul v (mapv #(- 1 %) v))
     (mapv (fn [x] (/ 1.0 (+ 1.0 (Math/exp (- (double x)))))) v))))

(defn tanh-act
  "Tanh over vector `v`; with `deriv?` returns 1 - v^2."
  ([v] (tanh-act v false))
  ([v deriv?]
   (if deriv?
     (mapv #(- 1 (* % %)) v)
     (mapv #(Math/tanh (double %)) v))))

(defn softmax
  "Softmax over vector `v`."
  [v]
  (let [m (apply max v)
        ex (mapv #(Math/exp (- % m)) v)
        s (reduce + ex)]
    (mapv #(/ % s) ex)))

(defn- init-matrix
  "Random rows x cols matrix, uniform(-1,1) scaled by sqrt(6/(in+out))."
  [rng rows cols]
  (let [s (Math/sqrt (/ 6.0 (+ rows cols)))]
    (vec (for [_ (range rows)]
           (vec (for [_ (range cols)] (* s (- (* 2 (.nextDouble rng)) 1))))))))

(defn make-lstm
  "Builds an LSTM model for `input-data` string.
  Options: :hidden-size (25), :epochs (100), :learning-rate (0.05),
  :seed (nil for nondeterministic; any long seeds the RNG)."
  [input-data & {:keys [hidden-size epochs learning-rate seed]
                 :or {hidden-size 25 epochs 100 learning-rate 0.05 seed nil}}]
  (let [data (.toLowerCase ^String input-data)
        vocab (vec (sort (set data)))
        c2i (zipmap vocab (range))
        i2c (zipmap (range) vocab)
        rng (if (nil? seed) (java.util.Random.) (java.util.Random. (long seed)))
        h hidden-size
        v (count vocab)
        in-dim (+ v h)]
    {:input-data data :hidden-size h :epochs epochs :lr learning-rate
     :vocab vocab :char->idx c2i :idx->char i2c
     :input-seq (subs data 0 (dec (count data)))
     :target-seq (subs data 1)
     :rng rng
     :wf (init-matrix rng h in-dim) :bf (zeros h)
     :wi (init-matrix rng h in-dim) :bi (zeros h)
     :wc (init-matrix rng h in-dim) :bc (zeros h)
     :wo (init-matrix rng h in-dim) :bo (zeros h)
     :wy (init-matrix rng v h) :by (zeros v)}))

(defn one-hot-encode
  "One-hot vector for character `ch` over the model vocabulary."
  [{:keys [vocab char->idx]} ch]
  (assoc (zeros (count vocab)) (char->idx ch) 1.0))

(defn- reset-cache
  "Fresh per-pass cache of timestep states."
  [h]
  {:combined {} :hidden {-1 (zeros h)} :cell {-1 (zeros h)}
   :forget {} :ig {} :cc {} :og {}})

(defn forward-pass
  "Runs `inputs` (seq of one-hot vectors) through the LSTM.
  Returns [model-with-cache outputs] where outputs are logit vectors."
  [{:keys [hidden-size wf bf wi bi wc bc wo bo wy by] :as model} inputs]
  (let [steps (count inputs)]
    (loop [t 0
           cache (reset-cache hidden-size)
           outs []]
      (if (>= t steps)
        [(assoc model :cache cache) outs]
        (let [combined (vec (concat (get (:hidden cache) (dec t)) (nth inputs t)))
              f (sigmoid-act (v+ (matvec wf combined) bf))
              i (sigmoid-act (v+ (matvec wi combined) bi))
              c-tilde (tanh-act (v+ (matvec wc combined) bc))
              o (sigmoid-act (v+ (matvec wo combined) bo))
              cell (v+ (emul f (get (:cell cache) (dec t))) (emul i c-tilde))
              hid (emul o (tanh-act cell))
              out (v+ (matvec wy hid) by)]
          (recur (inc t)
                 (-> cache
                     (assoc-in [:combined t] combined)
                     (assoc-in [:forget t] f)
                     (assoc-in [:ig t] i)
                     (assoc-in [:cc t] c-tilde)
                     (assoc-in [:og t] o)
                     (assoc-in [:cell t] cell)
                     (assoc-in [:hidden t] hid))
                 (conj outs out)))))))

(defn backward-pass
  "Backpropagates `errors` for `inputs`, updating weights in place
  functionally. Returns the updated model."
  [{:keys [hidden-size lr wf wi wc wo wy] :as model} errors inputs]
  (let [{:keys [combined hidden cell forget ig cc og]} (:cache model)
        h hidden-size
        T (count inputs)
        [dwf dbf dwi dbi dwc dbc dwo dbo dwy dby]
        (loop [t (dec T)
               dh (zeros h)
               dc (zeros h)
               acc [(m-scale wf 0.0) (zeros h)
                    (m-scale wi 0.0) (zeros h)
                    (m-scale wc 0.0) (zeros h)
                    (m-scale wo 0.0) (zeros h)
                    (m-scale wy 0.0) (zeros (count (first errors)))]]
          (if (neg? t)
            acc
            (let [[awf abf awi abi awc abc awo abo awy aby] acc
                  err (nth errors t)
                  hid (get hidden t)
                  cl (get cell t)
                  x (get combined t)
                  dwy (m+ awy (outer err hid))
                  dby (v+ aby err)
                  dh' (v+ (matvec (transpose wy) err) dh)
                  do (emul (emul (tanh-act cl) dh')
                           (sigmoid-act (get og t) true))
                  dwo (m+ awo (outer do x))
                  dbo (v+ abo do)
                  dcell (v+ (emul (emul (tanh-act (tanh-act cl) true)
                                        (get og t))
                                  dh')
                            dc)
                  df (emul (emul dcell (get cell (dec t)))
                           (sigmoid-act (get forget t) true))
                  di (emul (emul dcell (get cc t))
                           (sigmoid-act (get ig t) true))
                  dcc (emul (emul dcell (get ig t))
                            (tanh-act (get cc t) true))
                  dcomb (reduce v+ [(matvec (transpose wf) df)
                                    (matvec (transpose wi) di)
                                    (matvec (transpose wc) dcc)
                                    (matvec (transpose wo) do)])]
              (recur (dec t)
                     (subvec dcomb 0 h)
                     (emul (get forget t) dcell)
                     [(m+ awf (outer df x)) (v+ abf df)
                      (m+ awi (outer di x)) (v+ abi di)
                      (m+ awc (outer dcc x)) (v+ abc dcc)
                      dwo dbo dwy dby]))))
        grads (map clip-mat [dwf dbf dwi dbi dwc dbc dwo dbo dwy dby])
        [gwf gbf gwi gbi gwc gbc gwo gbo gwy gby] grads]
    (-> model
        (update :wf m+ (m-scale gwf lr)) (update :bf v+ (vscale gbf lr))
        (update :wi m+ (m-scale gwi lr)) (update :bi v+ (vscale gbi lr))
        (update :wc m+ (m-scale gwc lr)) (update :bc v+ (vscale gbc lr))
        (update :wo m+ (m-scale gwo lr)) (update :bo v+ (vscale gbo lr))
        (update :wy m+ (m-scale gwy lr)) (update :by v+ (vscale gby lr)))))

(defn train
  "Trains the model for its configured epochs. Returns updated model."
  [{:keys [epochs char->idx input-seq target-seq] :as model}]
  (loop [m model
         e 0]
    (if (>= e epochs)
      m
      (let [inputs (mapv #(one-hot-encode m %) input-seq)
            [m2 preds] (forward-pass m inputs)
            errors (mapv (fn [p target]
                           (let [s (softmax p)
                                 j (char->idx target)]
                             (assoc (vscale s -1.0) j (+ (- (nth s j)) 1))))
                         preds target-seq)]
        (recur (backward-pass m2 errors inputs) (inc e))))))

(defn- sample-index
  "Samples an index from probability vector `probs` using `rng`."
  [rng probs]
  (let [r (.nextDouble rng)]
    (loop [i 0
           acc 0.0]
      (let [acc' (+ acc (nth probs i))]
        (if (or (>= acc' r) (= i (dec (count probs))))
          i
          (recur (inc i) acc'))))))

(defn test-model
  "Generates one predicted character per input position by sampling
  the softmax outputs. Returns the prediction string."
  [{:keys [input-seq idx->char rng] :as model}]
  (let [[_ probs] (forward-pass model (mapv #(one-hot-encode model %) input-seq))]
    (apply str (map (fn [p] (idx->char (sample-index rng (softmax (vec p)))))
                    probs))))
