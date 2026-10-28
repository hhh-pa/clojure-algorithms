(ns clojure-algorithms.networking-flow.minimum-cut
  "Minimum s-t cut of a flow network via the Ford-Fulkerson algorithm.

  By the max-flow min-cut theorem, the value of a maximum flow equals the
  total capacity of the cheapest set of edges whose removal disconnects
  the sink from the source. This namespace runs Ford-Fulkerson to build
  the residual graph, then reports every original edge that is saturated
  (positive original capacity, zero residual capacity), matching the
  Python reference in this directory.

  Reference: https://en.wikipedia.org/wiki/Minimum_cut
  See also: https://en.wikipedia.org/wiki/Max-flow_min-cut_theorem")

(def test-graph
  "The classic 6-vertex example. Minimum 0-5 cut is
  [[1 3] [4 3] [4 5]] with total capacity 23."
  [[0 16 13 0 0 0]
   [0 0 10 12 0 0]
   [0 4 0 0 14 0]
   [0 0 9 0 0 20]
   [0 0 0 7 0 4]
   [0 0 0 0 0 0]])

(defn- valid-vertex?
  [graph v]
  (and (integer? v) (<= 0 v (dec (count graph)))))

(defn bfs
  "Breadth-first search in the residual `graph` from `source` to `sink`.

  Returns a map `{:found? boolean :parents vector}` where `:found?` is
  true when `sink` is reachable along positive-residual edges, and
  `:parents` records the BFS tree (-1 for unvisited vertices)."
  [graph source sink]
  (let [n (count graph)]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY source)
           visited (assoc (vec (repeat n false)) source true)
           parents (vec (repeat n -1))]
      (if (empty? queue)
        {:found? (boolean (get visited sink)) :parents parents}
        (let [u (peek queue)
              q (pop queue)]
          (if (= u sink)
            {:found? true :parents parents}
            (let [[q2 visited2 parents2]
                  (reduce (fn [[qq vv pp] v]
                            (if (and (not (vv v))
                                     (pos? (get-in graph [u v] 0)))
                              [(conj qq v)
                               (assoc vv v true)
                               (assoc pp v u)]
                              [qq vv pp]))
                          [q visited parents]
                          (range n))]
              (recur q2 visited2 parents2))))))))

(defn mincut
  "Returns the edges of a minimum s-t cut as a vector of `[from to]` pairs.

  `graph` is an adjacency matrix of capacities and is left unchanged;
  the algorithm works on an internal residual copy. Edges are reported
  in row-major order, matching the Python reference.

  Throws IllegalArgumentException when `source`/`sink` are out of range
  or equal (the Python reference would loop forever on source == sink)."
  [graph source sink]
  (when-not (and (valid-vertex? graph source) (valid-vertex? graph sink))
    (throw (IllegalArgumentException. "source/sink vertex out of range")))
  (when (= source sink)
    (throw (IllegalArgumentException. "source and sink must be different")))
  (let [residual (loop [r (mapv vec graph)]
                   (let [{:keys [found? parents]} (bfs r source sink)]
                     (if-not found?
                       r
                       (let [path-flow (loop [v sink
                                              flow Long/MAX_VALUE]
                                         (if (= v source)
                                           flow
                                           (recur (parents v)
                                                  (min flow
                                                       (get-in r [(parents v) v])))))
                             r' (loop [v sink
                                       acc r]
                                  (if (= v source)
                                    acc
                                    (let [u (parents v)]
                                      (recur u (-> acc
                                                   (update-in [u v] - path-flow)
                                                   (update-in [v u] + path-flow))))))]
                         (recur r')))))]
    (vec (for [i (range (count graph))
               j (range (count (graph i)))
               :when (and (pos? (get-in graph [i j]))
                          (zero? (get-in residual [i j])))]
           [i j]))))
