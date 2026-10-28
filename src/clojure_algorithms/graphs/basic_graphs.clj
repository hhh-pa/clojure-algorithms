(ns clojure-algorithms.graphs.basic-graphs
  "Basic graph routines: DFS, BFS, Dijkstra, topo sort, Floyd-Warshall,
   Prim, Kruskal and isolated nodes. This port returns values instead of
   printing, and omits the stdin-driven input helpers.")

(defn dfs
  "Returns DFS visit order from s (first-unvisited-child descent)."
  [g s]
  (loop [stack [s] visited #{s} order [s]]
    (if (empty? stack)
      order
      (if-let [nxt (first (remove visited (get g (peek stack) [])))]
        (recur (conj stack nxt) (conj visited nxt) (conj order nxt))
        (recur (pop stack) visited order)))))

(defn bfs
  "Returns BFS visit order from s."
  [g s]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY s)
         visited #{s} order [s]]
    (if (empty? queue)
      order
      (let [u (peek queue)
            fresh (remove visited (get g u []))]
        (recur (into (pop queue) fresh)
               (into visited fresh)
               (into order fresh))))))

(defn dijk
  "Returns {node shortest-distance} from s over weighted adjacency."
  [g s]
  (loop [heap (sorted-set [0 s])
         dist {s 0}
         known #{}]
    (if (empty? heap)
      dist
      (let [[d u] (first heap)
            heap (disj heap [d u])]
        (if (contains? known u)
          (recur heap dist known)
          (let [known (conj known u)
                [heap dist]
                (reduce (fn [[h ds] [v w]]
                          (if (and (not (contains? known v))
                                   (< (+ d w) (get ds v Long/MAX_VALUE)))
                            [(conj h [(+ d w) v]) (assoc ds v (+ d w))]
                            [h ds]))
                        [heap dist] (get g u []))]
            (recur heap dist known)))))))

(defn topo
  "Returns a topological ordering (Kahn's algorithm)."
  [g]
  (let [indeg (atom (into {} (map #(vector % 0) (keys g))))]
    (doseq [u (keys g) v (get g u [])]
      (swap! indeg update v inc))
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                       (filter #(zero? (get @indeg %)) (keys g)))
           order []]
      (if (empty? queue)
        order
        (let [v (peek queue)]
          (doseq [w (get g v [])]
            (swap! indeg update w dec))
          (recur (into (pop queue)
                       (filter #(zero? (get @indeg %)) (get g v [])))
                 (conj order v)))))))

(defn floyd-warshall
  "Returns the all-pairs shortest-path matrix for [matrix n]."
  [[a n]]
  (let [dist (atom (mapv vec a))]
    (doseq [k (range n) i (range n) j (range n)]
      (let [alt (+ (get-in @dist [i k]) (get-in @dist [k j]))]
        (when (> (get-in @dist [i j]) alt)
          (swap! dist assoc-in [i j] alt))))
    @dist))

(defn prim
  "Returns {node min-edge-weight} of the Prim tree from s."
  [g s]
  (loop [heap (sorted-set [0 s]) dist {s 0} known #{}]
    (if (empty? heap)
      dist
      (let [[d u] (first heap)
            heap (disj heap [d u])]
        (if (contains? known u)
          (recur heap dist known)
          (let [known (conj known u)
                [heap dist]
                (reduce (fn [[h ds] [v w]]
                          (if (and (not (contains? known v))
                                   (< w (get ds v Long/MAX_VALUE)))
                            [(conj h [w v]) (assoc ds v w)]
                            [h ds]))
                        [heap dist] (get g u []))]
            (recur heap dist known)))))))

(defn krusk
  "Returns MST edges from [[u v w] ...] edges over n 1-based nodes."
  [[edges n]]
  (let [parent (atom (into {} (map #(vector % %) (range 1 (inc n)))))
        find (fn find [x]
               (let [p (get @parent x)]
                 (if (= p x) x (let [r (find p)]
                                 (swap! parent assoc x r) r))))]
    (reduce (fn [mst [u v w :as e]]
              (let [pu (find u) pv (find v)]
                (if (= pu pv)
                  mst
                  (do (swap! parent assoc pu pv)
                      (conj mst e)))))
            [] (sort-by #(nth % 2) edges))))

(defn find-isolated-nodes
  "Returns the nodes with no outgoing edges."
  [graph]
  (vec (filter #(empty? (get graph %)) (keys graph))))
