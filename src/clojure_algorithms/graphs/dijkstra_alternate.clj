(ns clojure-algorithms.graphs.dijkstra-alternate
  "Dijkstra over an adjacency matrix (0 means no edge).")

(def inf-dist "Unreachable marker used by the original." 10000000)

(defn make-graph
  "Builds a zero-initialized vertices x vertices matrix graph."
  [vertices]
  {:vertices vertices
   :matrix (vec (repeat vertices (vec (repeat vertices 0))))})

(defn set-edge
  "Returns the graph with the u->v weight set."
  [graph u v w]
  (assoc-in graph [:matrix u v] w))

(defn minimum-distance
  "Index of the unvisited vertex with minimum distance."
  [{:keys [vertices]} distances-from-source visited]
  (loop [v 0 best 0 best-val 1e7]
    (if (>= v vertices)
      best
      (if (and (< (nth distances-from-source v) best-val)
               (false? (nth visited v)))
        (recur (inc v) v (nth distances-from-source v))
        (recur (inc v) best best-val)))))

(defn dijkstra
  "Returns shortest distances from source over the matrix graph."
  [{:keys [vertices matrix] :as graph} source]
  (loop [iter 0
         distances (assoc (vec (repeat vertices inf-dist)) source 0)
         visited (vec (repeat vertices false))]
    (if (>= iter vertices)
      distances
      (let [u (minimum-distance graph distances visited)
            visited (assoc visited u true)]
        (recur (inc iter)
               (reduce (fn [ds v]
                         (if (and (pos? (get-in matrix [u v]))
                                  (false? (nth visited v))
                                  (> (nth ds v)
                                     (+ (nth ds u) (get-in matrix [u v]))))
                           (assoc ds v (+ (nth ds u) (get-in matrix [u v])))
                           ds))
                       distances (range vertices))
               visited)))))
