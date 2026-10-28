(ns clojure-algorithms.graphs.minimum-spanning-tree-boruvka
  "Minimum spanning tree via Boruvka's algorithm with union-find.")

(defn make-graph
  "Builds an empty undirected weighted graph."
  [] {:adjacency {}})

(defn add-vertex [graph vertex]
  (if (contains? (:adjacency graph) vertex)
    graph (assoc-in graph [:adjacency vertex] {})))

(defn add-edge
  "Returns the graph with the weighted head<->tail edge added."
  [graph head tail weight]
  (if (= head tail)
    (-> graph (add-vertex head) (add-vertex tail))
    (-> graph
        (add-vertex head) (add-vertex tail)
        (assoc-in [:adjacency head tail] weight)
        (assoc-in [:adjacency tail head] weight))))

(defn build-graph
  "Builds a graph from vertices and [head tail weight] edges."
  [vertices edges]
  (reduce (fn [g e] (apply add-edge g e))
          (reduce add-vertex (make-graph) vertices) edges))

(defn distinct-weight
  "Rewrites weights to be distinct (required by Boruvka)."
  [graph]
  (let [seen (atom #{})
        edges (atom [])]
    (doseq [[tail ns] (:adjacency graph) [head w] ns]
      (when-not (contains? @seen [tail head])
        (swap! seen conj [head tail])
        (swap! edges conj [tail head w])))
    (let [sorted-edges (sort-by #(nth % 2) @edges)
          bumped (loop [es sorted-edges prev nil acc []]
                   (if (empty? es)
                     acc
                     (let [[h t w] (first es)
                           w (if (and prev (<= w prev)) (inc prev) w)]
                       (recur (rest es) w (conj acc [h t w])))))]
      (reduce (fn [g [h t w]]
                (-> g
                    (assoc-in [:adjacency h t] w)
                    (assoc-in [:adjacency t h] w)))
              graph bumped))))

(defn boruvka-mst
  "Returns the MST as a new graph."
  [graph]
  (let [vertices (vec (keys (:adjacency graph)))
        parent (atom (into {} (map #(vector % %) vertices)))
        rank (atom (into {} (map #(vector % 0) vertices)))
        find (fn find [x]
               (let [p (get @parent x)]
                 (if (= p x) x (let [r (find p)]
                                 (swap! parent assoc x r) r))))
        union (fn [a b]
                (let [ra (find a) rb (find b)]
                  (when (not= ra rb)
                    (let [rka (get @rank ra) rkb (get @rank rb)]
                      (cond (> rka rkb) (swap! parent assoc rb ra)
                            (< rka rkb) (swap! parent assoc ra rb)
                            :else (do (swap! parent assoc rb ra)
                                      (swap! rank assoc ra (inc rka))))))))
        seen (atom #{})
        edges (doall (for [[tail ns] (:adjacency graph) [head w] ns
                           :when (let [k [tail head]]
                                   (when-not (contains? @seen k)
                                     (swap! seen conj [head tail]) true))]
                       [tail head w]))
        ncomp (atom (count vertices))
        mst-edges (atom [])]
    (while (> @ncomp 1)
      (let [cheap (atom {})]
        (doseq [[h t w] edges]
          (let [s1 (find h) s2 (find t)]
            (when (not= s1 s2)
              (doseq [s [s1 s2]]
                (when (or (nil? (get @cheap s))
                          (> (nth (get @cheap s) 2) w))
                  (swap! cheap assoc s [h t w]))))))
        (doseq [[_ [h t w]] @cheap]
          (when (not= (find h) (find t))
            (union h t)
            (swap! mst-edges conj [h t w])
            (swap! ncomp dec)))))
    (build-graph [] @mst-edges)))
