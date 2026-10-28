(ns clojure-algorithms.neural-network.convolution-neural-network
  "Convolutional neural network for image recognition.

  Five stages: convolution, pooling, flatten, hidden, and output layers
  trained with backpropagation. Images and maps are plain vectors of
  row vectors. Model persistence uses EDN instead of pickle.
  (The Python original plots training error with matplotlib; this port
  returns the final MSE. Its `convolute` also calls an undefined
  `Expand_Mat` helper; this port flattens each window directly, which is
  the shape the training step requires.)"
  (:require [clojure.edn :as edn]))

(defn sigmoid-value
  "Sigmoid of a single number."
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- (double x))))))

(defn- sigmoid-mat
  "Applies sigmoid element-wise to a matrix."
  [m]
  (mapv (fn [row] (mapv sigmoid-value row)) m))

(defn make-cnn
  "Builds a CNN state map. `conv-spec` is [kernel-size num-kernels step],
  `size-pool` the pooling size, `bp1`/`bp2`/`bp3` the flatten/hidden/output
  sizes. Rates default to 0.2. Pass `rng` (java.util.Random) for tests."
  ([conv-spec size-pool bp1 bp2 bp3]
   (make-cnn conv-spec size-pool bp1 bp2 bp3 0.2 0.2 (java.util.Random.)))
  ([conv-spec size-pool bp1 bp2 bp3 rate-w rate-t]
   (make-cnn conv-spec size-pool bp1 bp2 bp3 rate-w rate-t (java.util.Random.)))
  ([[k num step] size-pool bp1 bp2 bp3 rate-w rate-t rng]
   (let [rand-comp #(- 0.5 (.nextDouble rng))]
     {:conv [k num] :step step :size-pool size-pool
      :bp1 bp1 :bp2 bp2 :bp3 bp3
      :rate-w rate-w :rate-t rate-t
      :w-conv (vec (for [_ (range num)]
                     (vec (for [_ (range k)]
                            (vec (for [_ (range k)] (rand-comp)))))))
      :wkj (vec (for [_ (range bp3)]
                  (vec (for [_ (range bp2)] (rand-comp)))))
      :vji (vec (for [_ (range bp2)]
                  (vec (for [_ (range bp1)] (rand-comp)))))
      :thre-conv (vec (for [_ (range num)] (- (* 2 (.nextDouble rng)) 1)))
      :thre-bp2 (vec (for [_ (range bp2)] (- (* 2 (.nextDouble rng)) 1)))
      :thre-bp3 (vec (for [_ (range bp3)] (- (* 2 (.nextDouble rng)) 1)))})))

(defn save-model
  "Writes the network map to `path` as EDN."
  [net path]
  (spit path (pr-str (select-keys net [:conv :step :size-pool :bp1 :bp2 :bp3
                                       :rate-w :rate-t :w-conv :wkj :vji
                                       :thre-conv :thre-bp2 :thre-bp3]))))

(defn read-model
  "Reads a network map previously written by `save-model`."
  [path]
  (edn/read-string (slurp path)))

(defn- submatrix
  "Extracts a k x k window from `m` at row `i`, col `j`."
  [m i j k]
  (mapv #(subvec % j (+ j k)) (subvec m i (+ i k))))

(defn- flatten-mat
  "Flattens a matrix to a row-major vector."
  [m]
  (vec (apply concat m)))

(defn convolute
  "Convolves square image `data` with the network kernels.
  Returns [focus-matrix featuremaps] where focus-matrix has one
  flattened-window row per window position."
  [{:keys [conv step w-conv thre-conv]} data]
  (let [[k num] conv
        n (count data)
        m (int (inc (/ (- n k) step)))
        foci (vec (for [i (range 0 (inc (- n k)) step)
                        j (range 0 (inc (- n k)) step)]
                    (submatrix data i j k)))
        maps (vec (for [ki (range num)]
                    (let [w (nth w-conv ki)
                          t (nth thre-conv ki)
                          feats (mapv (fn [f]
                                        (sigmoid-value
                                         (- (reduce + (map * (flatten-mat f) (flatten-mat w))) t)))
                                      foci)]
                      (mapv vec (partition m feats)))))]
    [(mapv flatten-mat foci) maps]))

(defn pooling
  "Pools `featuremaps` with block size `p`.
  `pool-type` is :average (default) or :max."
  ([featuremaps p] (pooling featuremaps p :average))
  ([featuremaps p pool-type]
   (let [agg (if (= pool-type :max)
               (fn [xs] (apply max xs))
               (fn [xs] (/ (reduce + xs) (count xs))))
         pool-one (fn [fmap]
                    (let [n (count fmap)
                          q (quot n p)]
                      (mapv (fn [bi]
                              (mapv (fn [bj]
                                      (let [block (vec (for [di (range p)
                                                             dj (range p)]
                                                         (get-in fmap [(+ (* bi p) di)
                                                                       (+ (* bj p) dj)])))]
                                        (agg block)))
                                    (range q)))
                            (range q))))]
     (mapv pool-one featuremaps))))

(defn expand-features
  "Flattens pooled feature maps to a single vector."
  [pooled]
  (vec (mapcat flatten-mat pooled)))

(defn- matvec
  "Multiplies matrix `m` by column vector `v`."
  [m v]
  (mapv (fn [row] (reduce + (map * row v))) m))

(defn- transpose
  "Transposes a matrix."
  [m]
  (apply mapv vector m))

(defn gradient-from-pool
  "Spreads flat pooled gradients `pd-pool` back to convolution-map
  resolution and multiplies by the sigmoid derivative of `out-maps`.
  Returns one gradient matrix per map."
  [out-maps pd-pool num size-map size-pool]
  (let [q (quot size-map size-pool)]
    (loop [ki 0
           off 0
           acc []]
      (if (>= ki num)
        acc
        (let [pd1 (vec (for [i (range size-map)]
                         (vec (for [j (range size-map)]
                                (nth pd-pool (+ off
                                                (* (quot i size-pool) q)
                                                (quot j size-pool)))))))
              om (nth out-maps ki)
              pd2 (mapv (fn [r1 r2] (mapv (fn [a o] (* a o (- 1 o))) r1 r2)) pd1 om)]
          (recur (inc ki) (+ off (* q q)) (conj acc pd2)))))))

(defn- forward-pass
  "Runs `data` through convolution, pooling, and dense layers.
  Returns a map of intermediates plus :output."
  [{:keys [size-pool vji wkj thre-bp2 thre-bp3] :as net} data]
  (let [[focus conved] (convolute net data)
        pooled (pooling conved size-pool)
        in (expand-features pooled)
        out2 (mapv sigmoid-value (mapv - (matvec vji in) thre-bp2))
        out3 (mapv sigmoid-value (mapv - (matvec wkj out2) thre-bp3))]
    {:focus focus :conved conved :pooled pooled :in in :out2 out2 :output out3}))

(defn train
  "Trains on `datas-train`/`datas-teach` for at most `n-repeat` epochs
  or until MSE drops below `error-accuracy`.
  Returns [updated-net final-mse]."
  [net patterns datas-train datas-teach n-repeat error-accuracy]
  (loop [n net
         rp 0
         mse 10000.0]
    (if (or (>= rp n-repeat) (< mse error-accuracy))
      [n mse]
      (let [[n' errors]
            (reduce (fn [[nc ec] [train teach]]
                      (let [{:keys [focus conved in out2 output]}
                            (forward-pass nc train)
                            {:keys [vji wkj thre-conv thre-bp2 thre-bp3
                                    rate-w rate-t size-pool conv]} nc
                            [k num] conv
                            pd-k (mapv (fn [t o] (* (- t o) o (- 1 o))) teach output)
                            pd-j (mapv (fn [c o] (* c o (- 1 o)))
                                       (matvec (transpose wkj) pd-k) out2)
                            pd-i (matvec (transpose vji) pd-j)
                            pd-pooled (mapv #(/ % (* size-pool size-pool)) pd-i)
                            pd-all (gradient-from-pool conved pd-pooled num
                                                       (count (first conved)) size-pool)
                            w-conv' (mapv (fn [w pd tki]
                                            (let [flat (flatten-mat pd)
                                                  dw (matvec (transpose focus) flat)]
                                              [(mapv vec (partition k (mapv (fn [a b] (+ a (* rate-w b)))
                                                                            (flatten-mat w) dw)))
                                               (- tki (* rate-t (reduce + flat)))]))
                                          (:w-conv nc) pd-all thre-conv)]
                        [(assoc nc
                                :w-conv (mapv first w-conv')
                                :thre-conv (mapv second w-conv')
                                :wkj (mapv (fn [rw pk] (mapv (fn [w o] (+ w (* rate-w pk o))) rw out2))
                                           wkj pd-k)
                                :vji (mapv (fn [rv pj] (mapv (fn [w x] (+ w (* rate-w pj x))) rv in))
                                           vji pd-j)
                                :thre-bp3 (mapv (fn [t pk] (- t (* rate-t pk))) thre-bp3 pd-k)
                                :thre-bp2 (mapv (fn [t pj] (- t (* rate-t pj))) thre-bp2 pd-j))
                         (+ ec (reduce + (map #(Math/abs (- %1 %2)) teach output)))]))
                    [n 0.0]
                    (map vector datas-train datas-teach))]
        (recur n' (inc rp) (/ errors patterns))))))

(defn- round3
  "Rounds `x` to three decimals."
  [x]
  (/ (Math/round (* x 1000.0)) 1000.0))

(defn predict
  "Predicts outputs for `datas-test`. Returns a vector of rounded rows."
  [net datas-test]
  (mapv (fn [d] (mapv round3 (:output (forward-pass net d)))) datas-test))

(defn convolution
  "Returns [featuremaps pooled] for image `data` (inspection helper)."
  [net data]
  (let [[_ conved] (convolute net data)]
    [conved (pooling conved (:size-pool net))]))
