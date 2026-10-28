(ns clojure-algorithms.sorts.odd-even-transposition-parallel
  "Odd-even transposition sort with parallel phases.

  The Python original spawns one OS process per element; this port keeps
  the phase structure but evaluates each phase's disjoint compare-swaps
  with `pmap` over the pair starts. Ported from TheAlgorithms/Python.")

(defn- apply-phase
  "One transposition phase over vector `v`: pairs (i, i+1) for i in
  `starts` are compare-swapped concurrently with `pmap`."
  [v starts]
  (let [swaps (into {} (pmap (fn [i]
                               (when (and (< (inc i) (count v))
                                          (pos? (compare (nth v i)
                                                         (nth v (inc i)))))
                                 [i (nth v (inc i))
                                  (inc i) (nth v i)]))
                             starts))]
    (reduce (fn [a [k val]] (assoc a k val)) v (partition 2 (apply concat
                                                                   (filter some?
                                                                           (map (fn [i]
                                                                                  (get swaps i))
                                                                                [])))))))

(defn odd-even-transposition
  "Sorts `coll` with n odd-even transposition phases. Returns a vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (if (<= n 1)
      v
      (loop [a v phase 0]
        (if (>= phase n)
          a
          (let [starts (range (mod phase 2) (dec n) 2)
                swaps (doall (pmap (fn [i]
                                     (when (pos? (compare (nth a i)
                                                          (nth a (inc i))))
                                       [i (nth a (inc i))
                                        (inc i) (nth a i)]))
                                   starts))]
            (recur (reduce (fn [b s]
                             (if s
                               (assoc b (nth s 0) (nth s 1)
                                      (nth s 2) (nth s 3))
                               b))
                           a swaps)
                   (inc phase))))))))
