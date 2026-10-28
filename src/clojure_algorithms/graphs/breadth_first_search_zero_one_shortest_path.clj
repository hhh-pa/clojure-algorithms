(ns clojure-algorithms.graphs.breadth-first-search-zero-one-shortest-path
  "Shortest paths with 0/1 edge weights via deque-based BFS.")

(defn make-graph
  "Builds an empty size-vertex adjacency list."
  [size]
  {:size size :adj (vec (repeat size []))})

(defn add-edge
  "Returns the graph with the 0/1-weighted edge added."
  [{:keys [size] :as graph} from-vertex to-vertex weight]
  (when-not (#{0 1} weight)
    (throw (IllegalArgumentException. "Edge weight must be either 0 or 1.")))
  (when-not (and (<= 0 from-vertex (dec size)) (<= 0 to-vertex (dec size)))
    (throw (IllegalArgumentException. "Vertex indexes must be in [0; size).")))
  (update-in graph [:adj from-vertex] conj [to-vertex weight]))

(defn get-shortest-path
  "Returns the start->finish distance. Throws when unreachable."
  [{:keys [size adj]} start-vertex finish-vertex]
  (let [dist (atom (assoc (vec (repeat size nil)) start-vertex 0))
        dq (java.util.ArrayDeque. ^java.util.List [start-vertex])]
    (while (not (.isEmpty dq))
      (let [v (.pollFirst dq)
            d (nth @dist v)]
        (when (some? d)
          (doseq [[to w] (nth adj v)]
            (let [nd (+ d w)
                  old (nth @dist to)]
              (when (or (nil? old) (< nd old))
                (swap! dist assoc to nd)
                (if (zero? w) (.addFirst dq to) (.addLast dq to))))))))
    (if (nil? (nth @dist finish-vertex))
      (throw (IllegalArgumentException.
              "No path from start_vertex to finish_vertex."))
      (nth @dist finish-vertex))))
