(ns clojure-algorithms.dynamic-programming.knapsack
  "0-1 knapsack with integer weights: bottom-up DP, memoized
  recursion, and optimal-subset reconstruction.
  Ported from TheAlgorithms/Python dynamic_programming/knapsack.py.")

(defn knapsack
  "Solve knapsack of capacity `w` with weights `wt` and values `val`.
  Returns [optimal-value dp-table]."
  [w wt val n]
  (loop [i 1 dp (vec (repeat (inc n) (vec (repeat (inc w) 0))))]
    (if (> i n)
      [(get-in dp [n w]) dp]
      (recur (inc i)
             (reduce (fn [d cap]
                       (assoc-in d [i cap]
                                 (if (<= (nth wt (dec i)) cap)
                                   (max (+ (nth val (dec i))
                                           (get-in d [(dec i) (- cap (nth wt (dec i)))]))
                                        (get-in d [(dec i) cap]))
                                   (get-in d [(dec i) cap]))))
                     dp
                     (range 1 (inc w)))))))

(defn mf-knapsack
  "Memoized (memory-function) knapsack value for `i` items, capacity `j`."
  [i wt val j]
  (let [memo (atom {})]
    (letfn [(solve [i j]
              (if (contains? @memo [i j])
                (@memo [i j])
                (let [v (if (or (zero? i) (zero? j))
                          0
                          (if (< j (nth wt (dec i)))
                            (solve (dec i) j)
                            (max (solve (dec i) j)
                                 (+ (solve (dec i) (- j (nth wt (dec i))))
                                    (nth val (dec i))))))]
                  (swap! memo assoc [i j] v)
                  v)))]
      (solve i j))))

(defn- construct-solution
  "1-based indices of one optimal subset from `dp-table`."
  [dp-table wt i j acc]
  (if (or (zero? i) (zero? j))
    acc
    (if (= (get-in dp-table [(dec i) j]) (get-in dp-table [i j]))
      (recur dp-table wt (dec i) j acc)
      (recur dp-table wt (dec i) (- j (nth wt (dec i))) (conj acc i)))))

(defn knapsack-with-example-solution
  "Return [optimal-value optimal-index-set] for capacity `w`."
  [w wt val]
  (when-not (and (sequential? wt) (sequential? val))
    (throw (IllegalArgumentException.
            "Both the weights and values vectors must be either lists or tuples")))
  (let [num-items (count wt)]
    (when-not (= num-items (count val))
      (throw (IllegalArgumentException.
              (str "The number of weights must be the same as the number of values.\n"
                   "But got " num-items " weights and " (count val) " values"))))
    (doseq [[i weight] (map-indexed vector wt)]
      (when-not (integer? weight)
        (throw (IllegalArgumentException.
                (str "All weights must be integers but got weight of type "
                     (type weight) " at index " i)))))
    (let [[optimal dp-table] (knapsack w (vec wt) (vec val) num-items)]
      [optimal (construct-solution dp-table (vec wt) num-items w #{})])))
