(ns clojure-algorithms.graphs.graph-centrality
  "Central (minimal eccentricity) and median (maximal harmonic
   closeness) nodes via Floyd-Warshall shortest paths.")

(def inf "Infinity marker." Double/POSITIVE_INFINITY)

(defn initialize-distance-matrix
  "Builds the distance matrix; throws on non-positive weights."
  [graph number-of-nodes]
  (reduce (fn [m [node edges]]
            (reduce (fn [m [nbr w]]
                      (when (<= w 0)
                        (throw (IllegalArgumentException.
                                (str "Edge weight must be positive. Found " w
                                     " between nodes " node " and " nbr "."))))
                      (assoc-in m [node nbr] (double w)))
                    m edges))
          (mapv (fn [i] (mapv (fn [j] (if (= i j) 0.0 inf))
                              (range number-of-nodes)))
                (range number-of-nodes))
          graph))

(defn floyd-warshall-algorithm
  "Returns the all-pairs shortest-path matrix."
  [graph]
  (let [n (count graph)
        dist (atom (initialize-distance-matrix graph n))]
    (doseq [k (range n) i (range n) j (range n)]
      (let [alt (+ (get-in @dist [i k]) (get-in @dist [k j]))]
        (when (< alt (get-in @dist [i j]))
          (swap! dist assoc-in [i j] alt))))
    @dist))

(defn get-reachable-distances
  "Finite nonzero distances from a row."
  [distances]
  (vec (remove #(or (= % inf) (zero? %)) distances)))

(defn- min-by-key [k coll]
  (reduce (fn [a b] (if (neg? (compare (k b) (k a))) b a)) coll))

(defn- max-by-key [k coll]
  (reduce (fn [a b] (if (pos? (compare (k b) (k a))) b a)) coll))

(defn find-central-node
  "Node with minimal eccentricity, [idx value]; [-1 inf] when none."
  [eccentricities]
  (let [valid (vec (remove #(= (second %) inf) eccentricities))]
    (if (empty? valid)
      [-1 inf]
      (min-by-key (fn [[i e]] [e i]) valid))))

(defn find-median-node
  "Node with maximal closeness, [idx value]; [-1 inf] when none."
  [closenesses]
  (let [valid (vec (remove #(= (second %) inf) closenesses))]
    (if (empty? valid)
      [-1 inf]
      (max-by-key (fn [[i c]] [c (- i)]) valid))))

(defn find-central-and-median-node
  "Returns [[central-idx ecc] [median-idx closeness]]."
  [distance-matrix]
  (let [n (count distance-matrix)]
    (if (= n 1)
      [[0 0.0] [0 0.0]]
      (let [stats (mapv (fn [i]
                          (let [reach (get-reachable-distances
                                       (nth distance-matrix i))]
                            (if (empty? reach)
                              [inf inf]
                              [(apply max reach)
                               (reduce + (map #(/ 1 %) reach))])))
                        (range n))]
        [(find-central-node (map-indexed vector (map first stats)))
         (find-median-node (map-indexed vector (map second stats)))]))))
