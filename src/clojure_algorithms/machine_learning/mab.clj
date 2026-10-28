(ns clojure-algorithms.machine-learning.mab
  "Multi-Armed Bandit strategies.

  Strategies are immutable maps; update returns a new strategy. Selection
  draws from the strategy's embedded RNG (seedable via the makers for
  deterministic tests).

  Only the plotting demo needs matplotlib and is honestly stubbed.

  Reference: https://en.wikipedia.org/wiki/Multi-armed_bandit")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn- make-rng
  [seed]
  (if (some? seed) (java.util.Random. (long seed)) (java.util.Random.)))

(defn- argmax-first
  [xs]
  (first (keep-indexed (fn [i v] (when (= v (apply max xs)) i)) xs)))

(defn- gamma-sample
  "Gamma(shape, 1) sample for shape >= 1 (Marsaglia-Tsang)."
  [rng shape]
  (let [d (- shape (/ 1.0 3.0))
        c (/ 1.0 (Math/sqrt (* 9.0 d)))]
    (loop []
      (let [x (.nextGaussian rng)
            v (Math/pow (+ 1.0 (* c x)) 3)]
        (if (<= v 0)
          (recur)
          (let [u (.nextDouble rng)]
            (if (< (Math/log u)
                   (+ (/ (* x x) 2.0) d (- (* d v)) (* d (Math/log v))))
              (* d v)
              (recur))))))))

(defn- beta-sample
  "Beta(a, b) sample via two Gamma variates (a, b >= 1 here)."
  [rng a b]
  (let [g1 (gamma-sample rng a)
        g2 (gamma-sample rng b)]
    (/ g1 (+ g1 g2))))

(defn make-Bandit
  "Bandit with per-arm reward probabilities."
  ([probabilities] (make-Bandit probabilities nil))
  ([probabilities seed]
   {:kind :bandit
    :probabilities (vec probabilities)
    :num-arms (count probabilities)
    :rng (make-rng seed)}))

(defn Bandit-pull
  "Pull arm-index: 1 with that arm's probability, else 0."
  [bandit arm-index]
  (if (< (.nextDouble ^java.util.Random (:rng bandit))
         (nth (:probabilities bandit) arm-index))
    1
    0))

(defn make-Strategy
  "Base strategy (selects nothing; update is a no-op)."
  []
  {:kind :strategy})

(defn make-EpsilonGreedy
  "Epsilon-greedy strategy over num-arms arms."
  ([epsilon num-arms] (make-EpsilonGreedy epsilon num-arms nil))
  ([epsilon num-arms seed]
   {:kind :epsilon-greedy
    :epsilon epsilon
    :num-arms num-arms
    :counts (vec (repeat num-arms 0.0))
    :values (vec (repeat num-arms 0.0))
    :rng (make-rng seed)}))

(defn make-UCB
  "Upper-confidence-bound strategy over num-arms arms."
  [num-arms]
  {:kind :ucb
   :num-arms num-arms
   :counts (vec (repeat num-arms 0.0))
   :values (vec (repeat num-arms 0.0))
   :total-counts 0})

(defn make-ThompsonSampling
  "Thompson-sampling strategy over num-arms arms."
  ([num-arms] (make-ThompsonSampling num-arms nil))
  ([num-arms seed]
   {:kind :thompson-sampling
    :num-arms num-arms
    :successes (vec (repeat num-arms 0.0))
    :failures (vec (repeat num-arms 0.0))
    :rng (make-rng seed)}))

(defn make-RandomStrategy
  "Uniformly random strategy over num-arms arms."
  ([num-arms] (make-RandomStrategy num-arms nil))
  ([num-arms seed]
   {:kind :random-strategy
    :num-arms num-arms
    :rng (make-rng seed)}))

(defn make-GreedyStrategy
  "Purely greedy strategy over num-arms arms."
  [num-arms]
  {:kind :greedy-strategy
   :num-arms num-arms
   :counts (vec (repeat num-arms 0.0))
   :values (vec (repeat num-arms 0.0))})

(defmulti poly-select-arm
  "Select an arm index for the strategy."
  (fn [strategy] (:kind strategy)))

(defmulti poly-update
  "Return the strategy updated with arm-index's reward."
  (fn [strategy _arm-index _reward] (:kind strategy)))

(defmethod poly-select-arm :strategy [_] nil)
(defmethod poly-update :strategy [s _ _] s)

(defmethod poly-select-arm :epsilon-greedy
  [{:keys [epsilon num-arms values rng]}]
  (if (< (.nextDouble ^java.util.Random rng) epsilon)
    (.nextInt ^java.util.Random rng num-arms)
    (argmax-first values)))

(defmethod poly-update :epsilon-greedy
  [s arm-index reward]
  (let [n (inc (nth (:counts s) arm-index))
        v (+ (nth (:values s) arm-index)
             (/ (- reward (nth (:values s) arm-index)) n))]
    (assoc s :counts (assoc (:counts s) arm-index n)
           :values (assoc (:values s) arm-index v))))

(defmethod poly-select-arm :ucb
  [{:keys [num-arms counts values total-counts]}]
  (if (< total-counts num-arms)
    total-counts
    (argmax-first (mapv (fn [v c]
                          (+ v (Math/sqrt (/ (* 2 (Math/log total-counts)) c))))
                        values counts))))

(defmethod poly-update :ucb
  [s arm-index reward]
  (let [n (inc (nth (:counts s) arm-index))
        v (+ (nth (:values s) arm-index)
             (/ (- reward (nth (:values s) arm-index)) n))]
    (assoc s :counts (assoc (:counts s) arm-index n)
           :values (assoc (:values s) arm-index v)
           :total-counts (inc (:total-counts s)))))

(defmethod poly-select-arm :thompson-sampling
  [{:keys [num-arms successes failures rng]}]
  (argmax-first (mapv #(beta-sample rng (inc (nth successes %))
                                    (inc (nth failures %)))
                      (range num-arms))))

(defmethod poly-update :thompson-sampling
  [s arm-index reward]
  (if (= reward 1)
    (assoc s :successes (assoc (:successes s) arm-index
                               (inc (nth (:successes s) arm-index))))
    (assoc s :failures (assoc (:failures s) arm-index
                              (inc (nth (:failures s) arm-index))))))

(defmethod poly-select-arm :random-strategy
  [{:keys [num-arms rng]}]
  (.nextInt ^java.util.Random rng num-arms))

(defmethod poly-update :random-strategy [s _ _] s)

(defmethod poly-select-arm :greedy-strategy
  [{:keys [values]}]
  (argmax-first values))

(defmethod poly-update :greedy-strategy
  [s arm-index reward]
  (let [n (inc (nth (:counts s) arm-index))
        v (+ (nth (:values s) arm-index)
             (/ (- reward (nth (:values s) arm-index)) n))]
    (assoc s :counts (assoc (:counts s) arm-index n)
           :values (assoc (:values s) arm-index v))))

(def Strategy-select-arm poly-select-arm)
(def EpsilonGreedy-select-arm poly-select-arm)
(def UCB-select-arm poly-select-arm)
(def ThompsonSampling-select-arm poly-select-arm)
(def RandomStrategy-select-arm poly-select-arm)
(def GreedyStrategy-select-arm poly-select-arm)

(def Strategy-update poly-update)
(def EpsilonGreedy-update poly-update)
(def UCB-update poly-update)
(def ThompsonSampling-update poly-update)
(def RandomStrategy-update poly-update)
(def GreedyStrategy-update poly-update)

(defn test-mab-strategies
  "Behavioural checks for the strategies (mirrors the upstream self-test):
  greedy settles on the rewarded arm, epsilon-0 matches greedy, UCB tries
  every arm first, Thompson favours the successful arm, random stays valid."
  []
  (let [num-arms 3
        greedy (reduce (fn [s _] (poly-update s 2 1))
                       (make-GreedyStrategy num-arms) (range 10))
        greedy (-> greedy (poly-update 0 0) (poly-update 1 0))]
    (assert (= 2 (poly-select-arm greedy))))
  (let [num-arms 3
        eps (reduce (fn [s _] (poly-update s 1 1))
                    (make-EpsilonGreedy 0.0 num-arms 7) (range 10))
        eps (-> eps (poly-update 0 0) (poly-update 2 0))]
    (assert (= 1 (poly-select-arm eps))))
  (let [num-arms 3
        [seen _] (reduce (fn [[seen ucb] _]
                           (let [arm (poly-select-arm ucb)]
                             [(conj seen arm) (poly-update ucb arm 1)]))
                         [#{} (make-UCB num-arms)] (range num-arms))]
    (assert (= seen (set (range num-arms)))))
  (let [num-arms 3
        thompson (reduce (fn [s _]
                           (-> s
                               (poly-update 0 1)
                               (poly-update 1 0)
                               (poly-update 2 0)))
                         (make-ThompsonSampling num-arms 11) (range 20))
        selections (vec (repeatedly 50 #(poly-select-arm thompson)))]
    (assert (> (count (filter zero? selections)) (quot (count selections) 2))))
  (let [random-strategy (make-RandomStrategy 3 13)
        _ (poly-update random-strategy 0 1)
        arm (poly-select-arm random-strategy)]
    (assert (and (<= 0 arm) (< arm 3))))
  nil)

(defn demo-mab-strategies
  "STUB: stochastic simulation with a matplotlib comparison plot."
  [& _args]
  (unsupported! "plt"))
