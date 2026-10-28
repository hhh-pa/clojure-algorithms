(ns clojure-algorithms.machine-learning.forecasting.run
  "Forecasting voters plus a data safety checker.

  The linear-regression voter (normal equations), the interquartile-range
  checker and the safety checker are genuinely portable and fully ported.
  The SARIMAX, SVR and random-forest voters need statsmodels/sklearn and
  are honestly stubbed.")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn- solve-linear
  "Solve Ax = b by Gauss-Jordan elimination with partial pivoting."
  [a b]
  (let [n (count a)
        aug (mapv #(vec %1) (map #(conj (vec %1) (double %2)) a b))]
    (loop [m aug
           col 0]
      (if (>= col n)
        (mapv #(nth % n) m)
        (let [pivot (apply max-key #(Math/abs (double (nth (nth m %) col)))
                           (range col n))
              m (assoc m col (nth m pivot) pivot (nth m col))
              piv (nth (nth m col) col)
              m (assoc m col (mapv #(/ % piv) (nth m col)))
              m (reduce (fn [m r]
                          (if (= r col)
                            m
                            (let [factor (nth (nth m r) col)]
                              (assoc m r (mapv #(- %1 (* factor %2))
                                               (nth m r) (nth m col))))))
                        m (range n))]
          (recur m (inc col)))))))

(defn- mat-transpose
  [rows]
  (apply mapv vector rows))

(defn- mat-mat
  [a b]
  (let [bt (mat-transpose b)]
    (mapv (fn [row] (mapv (fn [col] (reduce + (map * row col))) bt)) a)))

(defn- mat-vec
  [a v]
  (mapv #(reduce + (map * % v)) a))

(defn linear-regression-prediction
  "OLS forecast via the normal equations beta = (X'X)^-1 X'y on rows
  [1 date event].

  NOTE: like the upstream code, the forecast adds the raw test event count
  (test_mtch[0] + beta[2]) instead of multiplying by beta[2]. Preserved."
  [train-dt train-usr train-mtch test-dt test-mtch]
  (let [x (mapv (fn [i dt] [1.0 (double dt) (double (nth train-mtch i))])
                (range) train-dt)
        xt (mat-transpose x)
        beta (solve-linear (mat-mat xt x) (mat-vec xt (mapv double train-usr)))]
    (Math/abs (+ (nth beta 0)
                 (* (double (first test-dt)) (nth beta 1))
                 (double (first test-mtch))
                 (nth beta 2)))))

(defn sarimax-predictor
  "STUB: needs statsmodels (SARIMAX state-space fitting)."
  [& _args]
  (unsupported! "SARIMAX"))

(defn support-vector-regressor
  "STUB: needs sklearn (SVR with RBF kernel)."
  [& _args]
  (unsupported! "SVR"))

(defn random-forest-regressor
  "STUB: needs sklearn (RandomForestRegressor)."
  [& _args]
  (unsupported! "RandomForestRegressor"))

(defn- percentile
  "p-th percentile by linear interpolation (like np.percentile default)."
  [sorted-xs p]
  (let [n (count sorted-xs)
        rank (* (/ p 100.0) (dec n))
        lo (int (Math/floor rank))
        hi (int (Math/ceil rank))]
    (+ (nth sorted-xs lo) (* (- rank lo) (- (nth sorted-xs hi)
                                            (nth sorted-xs lo))))))

(defn interquartile-range-checker
  "Low outlier fence: Q1 - 0.1 * IQR of the training values."
  [train-user]
  (let [sorted-xs (vec (sort train-user))
        q1 (percentile sorted-xs 25)
        q3 (percentile sorted-xs 75)]
    (double (- q1 (* (- q3 q1) 0.1)))))

(defn data-safety-checker
  "True when the predicted votes agree with the actual result.
  Throws :TypeError unless actual-result is a float.

  NOTE: like the upstream code, an over-predicting vote sets
  safe = not-safe + 1 instead of incrementing safe. Preserved."
  [list-vote actual-result]
  (when-not (or (instance? Double actual-result)
                (instance? Float actual-result))
    (throw (ex-info "Actual result should be float. Value passed is a list"
                    {:type :TypeError})))
  (loop [votes list-vote
         safe 0
         not-safe 0]
    (if (empty? votes)
      (> safe not-safe)
      (let [i (first votes)]
        (cond (> i actual-result)
              (recur (rest votes) (inc not-safe) not-safe)

              (<= (Math/abs (- (Math/abs (double i))
                               (Math/abs actual-result)))
                  0.1)
              (recur (rest votes) (inc safe) not-safe)

              :else
              (recur (rest votes) safe (inc not-safe)))))))
