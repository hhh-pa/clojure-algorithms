(ns clojure-algorithms.graphs.bellman-ford
  "Single-source shortest paths with negative weights (Bellman-Ford).
   Edges are maps {:src :dst :weight}.")

(defn check-negative-cycle
  "True when any edge can still be relaxed."
  [graph distance edge-count]
  (boolean
   (some (fn [j]
           (let [{:keys [src dst weight]} (nth graph j)]
             (and (not= (nth distance src) Double/POSITIVE_INFINITY)
                  (< (+ (nth distance src) weight) (nth distance dst)))))
         (range edge-count))))

(defn bellman-ford
  "Returns shortest distances from src. Throws when a negative cycle exists."
  [graph vertex-count edge-count src]
  (let [distance (atom (assoc (vec (repeat vertex-count
                                           Double/POSITIVE_INFINITY))
                              src 0.0))]
    (dotimes [_ (dec vertex-count)]
      (dotimes [j edge-count]
        (let [{:keys [src dst weight]} (nth graph j)
              du (nth @distance src)]
          (when (and (not= du Double/POSITIVE_INFINITY)
                     (< (+ du weight) (nth @distance dst)))
            (swap! distance assoc dst (+ du weight))))))
    (when (check-negative-cycle graph @distance edge-count)
      (throw (Exception. "Negative cycle found")))
    @distance))
