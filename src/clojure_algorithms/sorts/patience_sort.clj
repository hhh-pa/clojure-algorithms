(ns clojure-algorithms.sorts.patience-sort
  "Patience sort: deal into piles, then k-way merge the piles.
  Ported from TheAlgorithms/Python.")

(defn- bisect-left-tops
  "Leftmost pile index whose top is >= `x` (comparing pile tops)."
  [stacks x]
  (loop [lo 0 hi (count stacks)]
    (if (>= lo hi)
      lo
      (let [mid (quot (+ lo hi) 2)]
        (if (neg? (compare (peek (nth stacks mid)) x))
          (recur (inc mid) hi)
          (recur lo mid))))))

(defn patience-sort
  "Sorts `coll` with patience sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        stacks (reduce (fn [sts x]
                         (let [i (bisect-left-tops sts x)]
                           (if (= i (count sts))
                             (conj sts [x])
                             (update sts i conj x))))
                       [] v)
        runs (mapv #(vec (rseq %)) stacks)]
    (loop [heads (mapv seq runs) acc (transient [])]
      (let [live (keep-indexed (fn [i s] (when s [i (first s)])) heads)]
        (if (empty? live)
          (persistent! acc)
          (let [[mi mv] (reduce (fn [best [i x]]
                                  (if (neg? (compare x (second best)))
                                    [i x] best))
                                (first live) (rest live))]
            (recur (assoc heads mi (next (nth heads mi)))
                   (conj! acc mv))))))))
