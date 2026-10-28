(ns clojure-algorithms.graphs.dijkstra-algorithm
  "Single-source shortest paths over an undirected weighted graph.
   Unreachable vertices keep infinite distance (the Python original
   leaves them at 0 when absent from the adjacency map).")

(def inf "Infinity marker for unreachable vertices." Long/MAX_VALUE)

(defn make-graph
  "Builds an empty num-node graph."
  [num-nodes]
  {:num-nodes num-nodes :adj {}})

(defn add-edge
  "Returns the graph with the undirected u<->v weighted edge added."
  [graph u v w]
  (-> graph
      (update-in [:adj u] (fnil conj []) [v w])
      (update-in [:adj v] (fnil conj []) [u w])))

(defn dijkstra
  "Returns {:dist :par} shortest distances and parents from src."
  [{:keys [num-nodes adj]} src]
  (loop [heap (sorted-set [0 src])
         dist (assoc (vec (repeat num-nodes inf)) src 0)
         par (vec (repeat num-nodes -1))
         done #{}]
    (if (empty? heap)
      {:dist dist :par par}
      (let [[d u] (first heap)
            heap (disj heap [d u])]
        (if (contains? done u)
          (recur heap dist par done)
          (let [done (conj done u)
                [heap dist par]
                (reduce (fn [[h ds ps] [v w]]
                          (let [nd (+ (nth ds u) w)]
                            (if (< nd (nth ds v))
                              [(conj (if (= (nth ds v) inf)
                                       h (disj h [(nth ds v) v]))
                                     [nd v])
                               (assoc ds v nd) (assoc ps v u)]
                              [h ds ps])))
                        [heap dist par] (get adj u []))]
            (recur heap dist par done)))))))

(defn shortest-path
  "Returns {:path :cost} from src to dest using a dijkstra result."
  [{:keys [adj]} {:keys [par]} src dest]
  (let [path (loop [t dest acc (list dest)]
               (if (= (nth par t) -1)
                 (vec acc)
                 (recur (nth par t) (cons (nth par t) acc))))
        cost (reduce + 0 (map (fn [a b]
                                (second (first (filter #(= (first %) b)
                                                       (get adj a [])))))
                              path (rest path)))]
    {:path path :cost cost}))
