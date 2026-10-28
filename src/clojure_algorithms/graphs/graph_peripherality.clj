(ns clojure-algorithms.graphs.graph-peripherality
  "Peripheral (maximal eccentricity) and far (maximal farness) nodes
   via Floyd-Warshall shortest paths.")

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

(defn- max-by-key [k coll]
  (reduce (fn [a b] (if (pos? (compare (k b) (k a))) b a)) coll))

(defn find-peripheral-node
  "Node with maximal eccentricity, [idx value]."
  [eccentricities]
  (if (empty? eccentricities)
    [-1 inf]
    (max-by-key (fn [[i e]] [e (- i)]) eccentricities)))

(defn find-far-node
  "Node with maximal farness, [idx value]."
  [farnesses]
  (if (empty? farnesses)
    [-1 inf]
    (max-by-key (fn [[i f]] [f (- i)]) farnesses)))

(defn find-peripheral-and-far-node
  "Returns [[peripheral-idx ecc] [far-idx farness]]."
  [distance-matrix]
  (let [n (count distance-matrix)]
    (if (= n 1)
      [[0 0.0] [0 0.0]]
      (let [stats (mapv (fn [i]
                          (let [reach (get-reachable-distances
                                       (nth distance-matrix i))]
                            (if (empty? reach)
                              [inf inf]
                              [(apply max reach) (reduce + reach)])))
                        (range n))]
        (if (every? #(= (first %) inf) stats)
          [[-1 inf] [-1 inf]]
          [(find-peripheral-node (map-indexed vector (map first stats)))
           (find-far-node (map-indexed vector (map second stats)))])))))
