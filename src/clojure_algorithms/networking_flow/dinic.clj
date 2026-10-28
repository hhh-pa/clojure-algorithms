(ns clojure-algorithms.networking-flow.dinic
  "Dinic's algorithm for the maximum-flow problem.

  Dinic's algorithm repeatedly builds a level graph with breadth-first
  search (shortest augmenting paths in edges) and then saturates a
  blocking flow on that level graph with depth-first search. Grouping
  augmenting paths by length gives O(V^2 * E) worst case, and O(E*sqrt(V))
  on unit-capacity networks.

  Graphs are plain immutable maps of the form
  `{:size n :adj [...] :edges [...]}` where `:adj` holds, per vertex, the
  indices of outgoing edges, and `:edges` holds `[destination
  residual-capacity]` pairs with edge `i` and its reverse `i ^ 1` always
  created together. Build one with `make-graph`/`add-edge`, then call
  `max-flow`. The input graph is never mutated.

  Reference: https://en.wikipedia.org/wiki/Dinic%27s_algorithm")

(defn make-graph
  "Creates an empty directed flow network with `n` vertices (0..n-1)."
  [n]
  (when (or (not (integer? n)) (<= n 0))
    (throw (IllegalArgumentException. "number of vertices must be positive")))
  {:size n
   :adj (vec (repeat n []))
   :edges []})

(defn- valid-vertex?
  [graph v]
  (and (integer? v) (<= 0 v (dec (:size graph)))))

(defn add-edge
  "Adds a directed edge `source -> destination` with `capacity`.

  Returns a new graph; parallel edges are supported and their capacities
  add up. Throws IllegalArgumentException on negative capacity or on a
  vertex out of range."
  [graph source destination capacity]
  (when (neg? capacity)
    (throw (IllegalArgumentException. "capacity must be non-negative")))
  (when-not (and (valid-vertex? graph source)
                 (valid-vertex? graph destination))
    (throw (IllegalArgumentException. "vertex out of range")))
  (let [fwd-idx (count (:edges graph))]
    (-> graph
        (update :edges conj [destination capacity] [source 0])
        (update-in [:adj source] conj fwd-idx)
        (update-in [:adj destination] conj (inc fwd-idx)))))

(defn max-flow
  "Returns the maximum flow from `source` to `sink` in `graph`.

  The input graph is left unchanged. Throws IllegalArgumentException
  when a vertex is out of range or when source equals sink."
  [graph source sink]
  (let [size (:size graph)
        adj (:adj graph)]
    (when-not (and (valid-vertex? graph source) (valid-vertex? graph sink))
      (throw (IllegalArgumentException. "vertex out of range")))
    (when (= source sink)
      (throw (IllegalArgumentException. "source and sink must be different")))
    (let [edges (atom (:edges graph))
          infinity (inc (long (reduce + 0 (map second @edges))))]
      (letfn [(build-level [s]
                (loop [level (assoc (vec (repeat size -1)) s 0)
                       q (conj clojure.lang.PersistentQueue/EMPTY s)]
                  (if (empty? q)
                    level
                    (let [v (peek q)
                          q (pop q)
                          snapshot @edges]
                      (let [[level q]
                            (reduce (fn [[lvl qq] ei]
                                      (let [[to res] (snapshot ei)]
                                        (if (and (pos? res) (= -1 (lvl to)))
                                          [(assoc lvl to (inc (lvl v)))
                                           (conj qq to)]
                                          [lvl qq])))
                                    [level q]
                                    (adj v))]
                        (recur level q))))))
              (send-flow [v pushed level progress]
                (if (= v sink)
                  pushed
                  (loop []
                    (let [idx (get @progress v)
                          nbrs (adj v)]
                      (if (>= idx (count nbrs))
                        0
                        (let [ei (nbrs idx)
                              [to res] (@edges ei)]
                          (if (and (pos? res) (= (level to) (inc (level v))))
                            (let [f (send-flow to
                                               (min (long pushed) (long res))
                                               level
                                               progress)]
                              (if (pos? f)
                                (do
                                  (swap! edges
                                         (fn [es]
                                           (let [[t1 c1] (es ei)
                                                 ri (bit-xor ei 1)
                                                 [t2 c2] (es ri)]
                                             (assoc es
                                                    ei [t1 (- c1 f)]
                                                    ri [t2 (+ c2 f)]))))
                                  f)
                                (do (swap! progress update v inc)
                                    (recur))))
                            (do (swap! progress update v inc)
                                (recur)))))))))]
        (loop [flow 0
               level (build-level source)]
          (if (= -1 (level sink))
            flow
            (let [progress (atom (vec (repeat size 0)))
                  pushed (loop [acc 0]
                           (let [f (send-flow source infinity level progress)]
                             (if (zero? f)
                               acc
                               (recur (+ acc f)))))]
              (recur (+ flow pushed) (build-level source)))))))))
