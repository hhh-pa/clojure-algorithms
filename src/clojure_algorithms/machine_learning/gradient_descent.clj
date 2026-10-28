(ns clojure-algorithms.machine-learning.gradient-descent
  "Gradient descent for the cost of a linear hypothesis function.

  The model has a bias term plus one weight per input component; the
  parameter vector is module-level mutable state, as in the upstream code.")

(def train-data
  "Input/output training pairs."
  [[[5 2 3] 15] [[6 5 9] 25] [[11 12 13] 41] [[1 1 1] 8] [[11 12 13] 41]])

(def test-data
  "Input/output test pairs."
  [[[515 22 13] 555] [[61 35 49] 150]])

(def parameter-vector
  "Mutable model parameters: [bias w1 w2 w3]. Reset to [2 4 1 5] to replay
  the documented examples after running gradient descent."
  (atom [2 4 1 5]))

(def m
  "Number of training examples."
  (count train-data))

(def LEARNING-RATE 0.009)

(defn output
  "Output of example-no from the \"train\" or \"test\" set; nil otherwise."
  [example-no data-set]
  (cond (= data-set "train") (second (nth train-data example-no))
        (= data-set "test") (second (nth test-data example-no))
        :else nil))

(defn -hypothesis-value
  "Hypothesis value for an input tuple (bias term added separately).

  >>> (-hypothesis-value [5 2 3])
  39"
  [data-input-tuple]
  (let [params @parameter-vector]
    (+ (reduce + (map * data-input-tuple (subvec params 1)))
       (first params))))

(defn calculate-hypothesis-value
  "Hypothesis value of example-no from the given set; nil otherwise."
  [example-no data-set]
  (cond (= data-set "train")
        (-hypothesis-value (first (nth train-data example-no)))
        (= data-set "test")
        (-hypothesis-value (first (nth test-data example-no)))
        :else nil))

(defn -error
  "Hypothesis value minus actual output for one example (default: train).

  >>> (-error 0)
  24"
  ([example-no] (-error example-no "train"))
  ([example-no data-set]
   (- (calculate-hypothesis-value example-no data-set)
      (output example-no data-set))))

(defn summation-of-cost-derivative
  "Sum of cost derivatives over the first end examples (default: all).
  Index -1 selects the bias parameter.

  >>> (summation-of-cost-derivative -1)
  243"
  ([index] (summation-of-cost-derivative index m))
  ([index end]
   (reduce + (for [i (range end)]
               (if (= index -1)
                 (-error i)
                 (* (-error i) (nth (first (nth train-data i)) index)))))))

(defn get-cost-derivative
  "Mean cost derivative with respect to parameter index (-1 for the bias).

  >>> (get-cost-derivative -1)
  48.6"
  [index]
  (/ (double (summation-of-cost-derivative index m)) m))

(defn- all-close?
  "True when every pair is within atol + rtol * |b| (like np.allclose)."
  [a b atol rtol]
  (every? true? (map #(<= (Math/abs (- (double %1) (double %2)))
                          (+ atol (* rtol (Math/abs (double %2)))))
                     a b)))

(defn run-gradient-descent
  "Descend until the parameter vector stops changing (allclose with
  atol 2e-6, rtol 0). Mutates parameter-vector and prints iterations."
  []
  (loop [j 1]
    (let [temp (mapv (fn [i]
                       (- (nth @parameter-vector i)
                          (* LEARNING-RATE (get-cost-derivative (dec i)))))
                     (range (count @parameter-vector)))]
      (if (all-close? @parameter-vector temp 0.000002 0)
        (println ["Number of iterations:" j])
        (do (reset! parameter-vector temp)
            (recur (inc j)))))))

(defn test-gradient-descent
  "Print actual vs hypothesis outputs for the test set."
  []
  (doseq [i (range (count test-data))]
    (println ["Actual output value:" (output i "test")])
    (println ["Hypothesis output:" (calculate-hypothesis-value i "test")])))
