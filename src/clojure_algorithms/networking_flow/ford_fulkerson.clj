(ns clojure-algorithms.networking-flow.ford-fulkerson
  "Ford-Fulkerson algorithm for the maximum-flow problem.

  Takes an adjacency matrix of non-negative integer capacities and
  repeatedly augments flow along shortest (BFS) source-to-sink paths in
  the residual graph until the sink is unreachable.

  Reference: https://en.wikipedia.org/wiki/Ford%E2%80%93Fulkerson_algorithm")

(def classic-graph
  "The classic 6-vertex CLRS-style example. Max flow from 0 to 5 is 23."
  [[0 16 13 0 0 0]
   [0 0 10 12 0 0]
   [0 4 0 0 14 0]
   [0 0 9 0 0 20]
   [0 0 0 7 0 4]
   [0 0 0 0 0 0]])

(defn- valid-vertex?
  [graph v]
  (and (integer? v) (<= 0 v (dec (count graph)))))

(defn breadth-first-search
  "Breadth-first search for an augmenting path in the residual `graph`.

  `graph` is an adjacency matrix of residual capacities, `source` the
  start vertex and `sink` the target vertex.

  Returns a map `{:found? boolean :parents vector}` where `:found?` is
  true when `sink` is reachable from `source` along positive-residual
  edges, and `:parents` records the BFS tree (-1 for unvisited/unparented
  vertices, `source` has parent -1)."
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

(defn ford-fulkerson
  "Returns the maximum flow from `source` to `sink` in `graph`.

  `graph` is an adjacency matrix of non-negative capacities. It is not
  modified; the algorithm works on an internal residual copy.

  Throws IllegalArgumentException when `source`/`sink` are out of range
  or equal (the Python reference would loop forever on source == sink)."
  [graph source sink]
  (when-not (and (valid-vertex? graph source) (valid-vertex? graph sink))
    (throw (IllegalArgumentException. "source/sink vertex out of range")))
  (when (= source sink)
    (throw (IllegalArgumentException. "source and sink must be different")))
  (loop [residual (mapv vec graph)
         max-flow 0]
    (let [{:keys [found? parents]} (breadth-first-search residual source sink)]
      (if-not found?
        max-flow
        (let [path-flow (loop [v sink
                               flow Long/MAX_VALUE]
                          (if (= v source)
                            flow
                            (recur (parents v)
                                   (min flow
                                        (get-in residual [(parents v) v])))))
              residual' (loop [v sink
                               r residual]
                          (if (= v source)
                            r
                            (let [u (parents v)]
                              (recur u (-> r
                                           (update-in [u v] - path-flow)
                                           (update-in [v u] + path-flow))))))]
          (recur residual' (+ max-flow path-flow)))))))
