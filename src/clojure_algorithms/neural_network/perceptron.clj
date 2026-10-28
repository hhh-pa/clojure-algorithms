(ns clojure-algorithms.neural-network.perceptron
  "Perceptron classifier (oil analysis example).

  Trains weights with w = w + lr * (target - y) * x and classifies
  samples as P1 (-1) or P2 (1). State is an immutable map.
  See https://en.wikipedia.org/wiki/Perceptron")

(defn sign-fn
  "Threshold function: 1 when `u` >= 0, else -1."
  [u]
  (if (>= u 0) 1 -1))

(defn make-perceptron
  "Builds a perceptron state map. `sample` rows and `target` labels must
  be non-empty and of matching length. Options: :learning-rate (0.01),
  :epoch-number (1000), :bias (-1), :seed (0, nil for nondeterministic)."
  [sample target & {:keys [learning-rate epoch-number bias seed]
                    :or {learning-rate 0.01 epoch-number 1000 bias -1 seed 0}}]
  (when (empty? sample)
    (throw (ex-info "Sample data can not be empty" {})))
  (when (empty? target)
    (throw (ex-info "Target data can not be empty" {})))
  (when (not= (count sample) (count target))
    (throw (ex-info "Sample data and Target data do not have matching lengths" {})))
  {:sample (vec sample)
   :target (vec target)
   :learning-rate learning-rate
   :epoch-number epoch-number
   :bias bias
   :seed seed
   :weights nil})

(defn- with-bias
  "Prepends the bias term to a row."
  [state row]
  (into [(:bias state)] row))

(defn- dot
  "Dot product of two equal-length vectors."
  [a b]
  (reduce + (map * a b)))

(defn train
  "Trains until all samples classify correctly or epochs run out.
  Returns [trained-state epoch-count]."
  [{:keys [sample target learning-rate epoch-number bias seed] :as state}]
  (let [rng (if (nil? seed) (java.util.Random.) (java.util.Random. (long seed)))
        cols (count (first sample))
        rows (mapv #(with-bias state %) sample)
        init-w (into [bias] (vec (repeatedly cols #(.nextDouble rng))))]
    (loop [weights init-w
           epoch 0]
      (if (>= epoch epoch-number)
        [(assoc state :weights weights) epoch]
        (let [[weights' mis?] (reduce (fn [[w mis?] [row t]]
                                        (let [y (sign-fn (dot w row))]
                                          (if (= y t)
                                            [w mis?]
                                            [(mapv (fn [wi xi]
                                                     (+ wi (* learning-rate (- t y) xi)))
                                                   w row)
                                             true])))
                                      [weights false]
                                      (map vector rows target))
              epoch' (inc epoch)]
          (if mis?
            (recur weights' epoch')
            [(assoc state :weights weights') epoch']))))))

(defn classify
  "Classifies `sample` row as -1 (P1) or 1 (P2).
  The state must have been trained first."
  [{:keys [weights] :as state} sample]
  (when (nil? weights)
    (throw (ex-info "Perceptron must be trained before classification" {})))
  (sign-fn (dot weights (with-bias state sample))))

(def samples
  "Oil-analysis sample dataset (30 rows of 3 features)."
  [[-0.6508 0.1097 4.0009] [-1.4492 0.8896 4.4005] [2.0850 0.6876 12.0710]
   [0.2626 1.1476 7.7985] [0.6418 1.0234 7.0427] [0.2569 0.6730 8.3265]
   [1.1155 0.6043 7.4446] [0.0914 0.3399 7.0677] [0.0121 0.5256 4.6316]
   [-0.0429 0.4660 5.4323] [0.4340 0.6870 8.2287] [0.2735 1.0287 7.1934]
   [0.4839 0.4851 7.4850] [0.4089 -0.1267 5.5019] [1.4391 0.1614 8.5843]
   [-0.9115 -0.1973 2.1962] [0.3654 1.0475 7.4858] [0.2144 0.7515 7.1699]
   [0.2013 1.0014 6.5489] [0.6483 0.2183 5.8991] [-0.1147 0.2242 7.2435]
   [-0.7970 0.8795 3.8762] [-1.0625 0.6366 2.4707] [0.5307 0.1285 5.6883]
   [-1.2200 0.7777 1.7252] [0.3957 0.1076 5.6623] [-0.1013 0.5989 7.1812]
   [2.4482 0.9455 11.2095] [2.0149 0.6192 10.9263] [0.2012 0.2611 5.4631]])

(def targets
  "Labels for `samples`: -1 is P1, 1 is P2."
  [-1 -1 -1 1 1 -1 1 -1 1 1 -1 1 -1 -1 -1 -1 1 1 1 1 -1 1 1 1 1 -1 -1 1 -1 1])
