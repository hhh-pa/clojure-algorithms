(ns clojure-algorithms.computer-vision.vision-transformer
  "Vision Transformer over nested vectors. Random projections take an
  optional java.util.Random for reproducibility.")

(defn- matmul
  [a b]
  (let [bt (apply mapv vector b)]
    (mapv (fn [row] (mapv (fn [col] (reduce + (map * row col))) bt)) a)))

(defn- randn-matrix
  [rng rows cols scale]
  (vec (repeatedly rows #(vec (repeatedly cols
                                          (fn [] (* (.nextGaussian ^java.util.Random rng) scale)))))))

(defn create-patches
  "Splits an HxWxC image into patches; returns [patches [nh nw]]."
  [image patch-size]
  (let [h (count image) w (count (first image)) c (count (ffirst image))]
    (when (or (zero? h) (zero? w) (zero? c))
      (throw (IllegalArgumentException. (str "Expected 3D image, got shape"))))
    (when (or (pos? (mod h patch-size)) (pos? (mod w patch-size)))
      (throw (IllegalArgumentException.
              (format "Image dimensions (%dx%d) must be divisible by patch_size (%d)"
                      h w patch-size))))
    (let [nh (quot h patch-size) nw (quot w patch-size)]
      [(vec (for [ph (range nh) pw (range nw)]
              (vec (for [r (range (* ph patch-size) (* (inc ph) patch-size))]
                     (vec (for [cc (range (* pw patch-size) (* (inc pw) patch-size))]
                            (get-in image [r cc])))))))
       [nh nw]])))

(defn patch-embedding
  "Flattens patches and projects them to embedding-dim."
  ([patches embedding-dim] (patch-embedding patches embedding-dim (java.util.Random.)))
  ([patches embedding-dim rng]
   (let [flat (mapv #(vec (apply concat (apply concat %))) patches)
         proj (randn-matrix rng (count (first flat)) embedding-dim 0.02)]
     (matmul flat proj))))

(defn add-positional-encoding
  "Prepends a CLS token and adds positional encodings."
  ([embeddings] (add-positional-encoding embeddings (java.util.Random.)))
  ([embeddings rng]
   (let [n (count embeddings) dim (count (first embeddings))
         pos (randn-matrix rng (inc n) dim 0.02)
         cls (vec (repeatedly dim #(* (.nextGaussian ^java.util.Random rng) 0.02)))
         with-cls (vec (cons cls embeddings))]
     (mapv #(mapv + %1 %2) with-cls pos))))

(defn- softmax-row
  [xs]
  (let [m (apply max xs)
        ex (mapv #(Math/exp (- % m)) xs)
        s (reduce + ex)]
    (mapv #(/ % s) ex)))

(defn attention-mechanism
  "Scaled dot-product attention; returns [output weights]."
  ([query key value] (attention-mechanism query key value nil))
  ([query key value mask]
   (let [dk (count (first query))
         scores (mapv (fn [q]
                        (mapv (fn [k] (/ (reduce + (map * q k)) (Math/sqrt dk)))
                              key))
                      query)
         scores (if mask
                  (mapv (fn [r m] (mapv (fn [s keep?] (if keep? s -1e9)) r m))
                        scores mask)
                  scores)
         weights (mapv softmax-row scores)]
     [(matmul weights value) weights])))

(defn layer-norm
  "Normalizes each row to zero mean and unit variance."
  ([embeddings] (layer-norm embeddings 1e-6))
  ([embeddings epsilon]
   (mapv (fn [row]
           (let [n (count row)
                 mean (/ (reduce + row) n)
                 var (/ (reduce + (map #(let [d (- % mean)] (* d d)) row)) n)
                 std (Math/sqrt var)]
             (mapv #(/ (- % mean) (+ std epsilon)) row)))
         embeddings)))

(defn- gelu
  [x]
  (* 0.5 x (+ 1 (Math/tanh (* (Math/sqrt (/ 2 Math/PI))
                              (+ x (* 0.044715 x x x)))))))

(defn feedforward-network
  "Position-wise feed-forward network with GELU."
  ([embeddings] (feedforward-network embeddings 3072 (java.util.Random.)))
  ([embeddings hidden-dim] (feedforward-network embeddings hidden-dim (java.util.Random.)))
  ([embeddings hidden-dim rng]
   (let [dim (count (first embeddings))
         w1 (randn-matrix rng dim hidden-dim 0.02)
         hidden (mapv #(mapv gelu %) (matmul embeddings w1))
         w2 (randn-matrix rng hidden-dim dim 0.02)]
     (matmul hidden w2))))

(defn transformer-encoder-block
  "One encoder block: attention + FFN with residual layer norms."
  ([embeddings] (transformer-encoder-block embeddings 12 3072 (java.util.Random.)))
  ([embeddings num-heads hidden-dim]
   (transformer-encoder-block embeddings num-heads hidden-dim (java.util.Random.)))
  ([embeddings _num-heads hidden-dim rng]
   (let [[attn _] (attention-mechanism embeddings embeddings embeddings)
         e1 (layer-norm (mapv #(mapv + %1 %2) embeddings attn))
         ffn (feedforward-network e1 hidden-dim rng)]
     (layer-norm (mapv #(mapv + %1 %2) e1 ffn)))))

(defn vision-transformer
  "Full ViT pipeline from image to class logits."
  [image & {:keys [patch-size embedding-dim num-layers num-heads hidden-dim
                   num-classes rng]
            :or {patch-size 16 embedding-dim 768 num-layers 12 num-heads 12
                 hidden-dim 3072 num-classes 1000 rng nil}}]
  (let [rng (or rng (java.util.Random.))
        [patches _] (create-patches image patch-size)
        embeddings (add-positional-encoding
                    (patch-embedding patches embedding-dim rng) rng)
        encoded (nth (iterate #(transformer-encoder-block % num-heads hidden-dim rng)
                              embeddings)
                     num-layers)
        cls (first encoded)
        cw (randn-matrix rng embedding-dim num-classes 0.02)]
    (mapv (fn [col] (reduce + (map * cls col))) (apply mapv vector cw))))
