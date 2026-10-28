(ns clojure-algorithms.graphs.johnson
  "All-pairs shortest paths via Johnson's algorithm.
   Graph maps node -> [[neighbour weight] ...].")

(def inf "Infinity marker." Double/POSITIVE_INFINITY)

(defn- bellman-ford-potentials [nodes edges]
  (let [dist (atom (into {} (map #(vector % 0.0) nodes)))]
    (dotimes [_ (dec (count nodes))]
      (doseq [[u v w] edges]
        (when (< (+ (get @dist u) w) (get @dist v))
          (swap! dist assoc v (+ (get @dist u) w)))))
    (doseq [[u v w] edges]
      (when (< (+ (get @dist u) w) (get @dist v))
        (throw (IllegalArgumentException. "Negative weight cycle detected"))))
    @dist))

(defn- dijkstra-reweighted [start nodes graph potentials]
  (loop [heap (sorted-set [0.0 start])
         dist (assoc (into {} (map #(vector % inf) nodes)) start 0.0)]
    (if (empty? heap)
      dist
      (let [[d u] (first heap)
            heap (disj heap [d u])]
        (if (> d (get dist u))
          (recur heap dist)
          (let [[heap dist]
                (reduce (fn [[h ds] [v w]]
                          (let [wp (+ w (get potentials u)
                                      (- (get potentials v)))
                                nd (+ d wp)]
                            (if (< nd (get ds v))
                              [(conj h [nd v]) (assoc ds v nd)]
                              [h ds])))
                        [heap dist] (get graph u []))]
            (recur heap dist)))))))

(defn johnson
  "Returns {u {v distance}} shortest paths. Throws on negative cycles."
  [graph]
  (let [nodes (vec (reduce into #{} (cons (keys graph)
                                          (map #(map first %) (vals graph)))))
        edges (for [[u ns] graph [v w] ns] [u v w])
        potentials (bellman-ford-potentials nodes edges)]
    (into {}
          (map (fn [s]
                 [s (let [dr (dijkstra-reweighted s nodes graph potentials)]
                      (into {}
                            (map (fn [v]
                                   [v (let [dp (get dr v)]
                                        (if (= dp inf)
                                          inf
                                          (+ dp (- (get potentials s))
                                             (get potentials v))))])
                                 nodes)))])
               nodes))))
