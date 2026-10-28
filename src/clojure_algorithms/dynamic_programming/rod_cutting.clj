(ns clojure-algorithms.dynamic-programming.rod-cutting
  "Rod-cutting: maximum revenue for a rod of length n.
  Naive recursive, top-down memoized and bottom-up variants.
  Ported from TheAlgorithms/Python dynamic_programming/rod_cutting.py.")

(defn- enforce-args
  "Validate rod length `n` against `prices`."
  [n prices]
  (when (neg? n)
    (throw (IllegalArgumentException.
            (str "n must be greater than or equal to 0. Got n = " n))))
  (when (> n (count prices))
    (throw (IllegalArgumentException.
            (str "Each integral piece of rod must have a corresponding price. "
                 "Got n = " n " but length of prices = " (count prices))))))

(defn naive-cut-rod-recursive
  "Maximum revenue via naive recursion, O(2^n)."
  [n prices]
  (enforce-args n prices)
  (if (zero? n)
    0
    (reduce max (for [i (range 1 (inc n))]
                  (+ (nth prices (dec i))
                     (naive-cut-rod-recursive (- n i) prices))))))

(defn top-down-cut-rod
  "Maximum revenue via memoized recursion, O(n^2)."
  [n prices]
  (enforce-args n prices)
  (let [memo (atom {})]
    (letfn [(solve [length]
              (cond
                (contains? @memo length) (@memo length)
                (zero? length) 0
                :else (let [best (reduce max
                                         (for [i (range 1 (inc length))]
                                           (+ (nth prices (dec i))
                                              (solve (- length i)))))]
                        (swap! memo assoc length best)
                        best)))]
      (solve n))))

(defn bottom-up-cut-rod
  "Maximum revenue via tabulation, O(n^2)."
  [n prices]
  (enforce-args n prices)
  (let [best (atom (assoc (vec (repeat (inc n) Double/NEGATIVE_INFINITY)) 0 0))]
    (doseq [i (range 1 (inc n))]
      (swap! best assoc i
             (reduce max (for [j (range 1 (inc i))]
                           (+ (nth prices (dec j)) (@best (- i j)))))))
    (let [result (@best n)]
      (if (== result (long result)) (long result) result))))
