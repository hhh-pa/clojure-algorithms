(ns clojure-algorithms.maths.triplet-sum
  "Given an array of integers and another integer target,
  we are required to find a triplet from the array such that it's sum is equal to
  the target.")

(defn make-dataset
  "Returns a random [array target] pair: 10 integers in [-1000, 1000] and a
  target in [-5000, 5000]."
  []
  [(vec (repeatedly 10 #(- (rand-int 2001) 1000)))
   (- (rand-int 10001) 5000)])

(def dataset
  "A random dataset shared by the timing comparison."
  (make-dataset))

(defn triplet-sum1
  "Returns a sorted triplet from `arr` summing to `target`, else [0 0 0].
  Naive implementation checking every permutation."
  [arr target]
  (let [v (vec arr)
        n (count v)]
    (or (some (fn [[i j k]]
                (let [t [(nth v i) (nth v j) (nth v k)]]
                  (when (== target (+ (t 0) (t 1) (t 2)))
                    (vec (sort t)))))
              (for [i (range n) j (range n) k (range n)
                    :when (and (not= i j) (not= i k) (not= j k))]
                [i j k]))
        [0 0 0])))

(defn triplet-sum2
  "Returns a sorted triplet from `arr` summing to `target`, else [0 0 0].
  Optimized implementation sorting first, then using two pointers."
  [arr target]
  (let [v (vec (sort arr))
        n (count v)]
    (or (some (fn [i]
                (loop [lo (inc i), hi (dec n)]
                  (when (< lo hi)
                    (let [s (+ (nth v i) (nth v lo) (nth v hi))]
                      (cond
                        (== s target) [(nth v i) (nth v lo) (nth v hi)]
                        (< s target) (recur (inc lo) hi)
                        :else (recur lo (dec hi)))))))
              (range (dec n)))
        [0 0 0])))

(defn solution-times
  "Upstream only benchmarks the two implementations with the timeit harness,
  which has no Clojure equivalent; kept as a stub so the public var still
  exists."
  [& _args]
  (throw (ex-info "solution-times requires the timeit harness"
                  {:type :UnsupportedOperation})))
