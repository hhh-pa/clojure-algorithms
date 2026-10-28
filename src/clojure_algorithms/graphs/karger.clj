(ns clojure-algorithms.graphs.karger
  "Graph partitioning via Karger's randomized contraction.")

(defn partition-graph
  "Contracts random edges until two nodes remain; returns the cutset
   as #{[node neighbour]}."
  [graph]
  (let [contracted (atom (into {} (map (fn [n] [n #{n}]) (keys graph))))
        work (atom (into {} (map (fn [[n ns]] [n (vec ns)]) graph)))]
    (while (> (count @work) 2)
      (let [nodes (vec (keys @work))
            u (nth nodes (rand-int (count nodes)))
            vs (get @work u)
            v (nth vs (rand-int (count vs)))
            uv (str u v)
            neighbours (vec (remove #{u v} (distinct (concat (get @work u)
                                                             (get @work v)))))]
        (swap! work assoc uv neighbours)
        (doseq [nb neighbours]
          (swap! work update nb (fnil conj []) uv))
        (swap! contracted assoc uv
               (clojure.set/union (get @contracted u) (get @contracted v)))
        (swap! work dissoc u v)
        (doseq [nb neighbours]
          (swap! work update nb
                 (fn [ns] (vec (remove #{u v} ns)))))))
    (let [[a b] (vec (vals @work))
          groups (vec (map @contracted (keys @work)))]
      (set (for [node (first groups)
                 neighbour (get graph node [])
                 :when (contains? (second groups) neighbour)]
             [node neighbour])))))
