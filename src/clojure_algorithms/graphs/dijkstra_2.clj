(ns clojure-algorithms.graphs.dijkstra-2
  "Dijkstra over an adjacency matrix with infinite for missing edges.")

(def inf "Infinity marker for unreachable vertices." Double/POSITIVE_INFINITY)

(defn min-dist
  "Index of the unvisited vertex with minimum distance, or -1."
  [mdist vset v]
  (loop [i 0 best -1 best-val inf]
    (if (>= i v)
      best
      (if (and (not (nth vset i)) (< (nth mdist i) best-val))
        (recur (inc i) i (nth mdist i))
        (recur (inc i) best best-val)))))

(defn dijkstra
  "Returns shortest distances from src over the v-vertex matrix graph."
  [graph v src]
  (loop [iter 0 mdist (assoc (vec (repeat v inf)) src 0.0)
         vset (vec (repeat v false))]
    (if (>= iter (dec v))
      mdist
      (let [u (min-dist mdist vset v)
            vset (assoc vset u true)]
        (recur (inc iter)
               (reduce (fn [md i]
                         (if (and (not (nth vset i))
                                  (not= (get-in graph [u i]) inf)
                                  (< (+ (nth md u) (get-in graph [u i]))
                                     (nth md i)))
                           (assoc md i (+ (nth md u) (get-in graph [u i])))
                           md))
                       mdist (range v))
               vset)))))

(defn print-dist
  "Prints the distance table."
  [dist v]
  (println "Vertex Distance")
  (doseq [i (range v)]
    (if (not= (nth dist i) inf)
      (println i "  " (long (nth dist i)))
      (println i "  " "INF"))))
