(ns clojure-algorithms.graphs.frequent-pattern-graph-miner
  "FP-GraphMiner: frequent-pattern mining over network graphs.")

(defn get-distinct-edge
  "Returns the distinct first-characters across the edge array."
  [edge-array]
  (vec (distinct (map #(subs % 0 1) (apply concat edge-array)))))

(defn get-bitcode
  "Bitcode string marking which graphs contain distinct-edge."
  [edge-array distinct-edge]
  (apply str (map (fn [row]
                    (if (some #(clojure.string/includes?
                                (subs % 0 1) distinct-edge)
                              row)
                      "1" "0"))
                  edge-array)))

(defn get-frequency-table
  "Returns [edge weight bitcode] rows sorted by descending weight."
  [edge-array]
  (->> (get-distinct-edge edge-array)
       (map (fn [e]
              (let [bit (get-bitcode edge-array e)]
                [e (count (filter #{\1} bit)) bit])))
       (sort-by second >)
       vec))

(defn get-nodes
  "Groups edges by bitcode: {bitcode [edges]}."
  [frequency-table]
  (reduce (fn [nodes [edge _ bit]]
            (update nodes bit (fnil conj []) edge))
          {} frequency-table))

(defn get-cluster
  "Groups nodes by bitcode weight: {weight {bitcode edges}}."
  [nodes]
  (reduce (fn [cluster [bit edges]]
            (assoc-in cluster [(count (filter #{\1} bit)) bit] edges))
          {} nodes))

(defn get-support
  "Support percentages per cluster key."
  [cluster]
  (mapv #(* % (/ 100.0 (count cluster))) (keys cluster)))

(defn preprocess
  "Splits \"edge-label\" strings into [edge label] pairs."
  [edge-array]
  (mapv (fn [row] (mapv #(clojure.string/split % #"-") row)) edge-array))

(defn construct-graph
  "Builds the FP-graph lattice from cluster and nodes maps."
  [cluster nodes]
  (let [top (apply max (keys cluster))
        graph (atom {})]
    (doseq [[_ edges] (get cluster top)]
      (swap! graph update [["Header"]] (fnil conj []) edges))
    (doseq [[_ edges] (get cluster top)]
      (swap! graph assoc [edges] [["Header"]]))
    (doseq [c1 (range 1 (dec (+ top 1)))]
      (when (contains? @graph nil)
        nil)
      (doseq [i (keys (get cluster c1 {}))]
        (loop [c2 (inc c1)]
          (if (>= c2 top)
            nil
            (let [hits (into []
                             (filter (fn [j]
                                       (= (bit-and (Integer/parseInt ^String i 2)
                                                   (Integer/parseInt ^String j 2))
                                          (Integer/parseInt ^String i 2))))
                             (keys (get cluster c2 {})))]
              (doseq [j hits]
                (swap! graph update [(get nodes i)]
                       (fnil conj []) (get nodes j)))
              (if (empty? hits)
                (recur (inc c2))
                nil))))))
    @graph))

(defn dfs-paths
  "Returns all DFS paths from start to end in graph."
  [graph start end]
  (let [paths (atom [])]
    (letfn [(walk [node path]
              (let [path (conj path node)]
                (when (= node end)
                  (swap! paths conj path))
                (doseq [next (get graph node [])]
                  (when-not (some #(= [next] %) path)
                    (walk [next] path)))))]
      (walk start []))
    @paths))

(defn freq-subgraphs-edge-list
  "Edge-pair lists for each frequent-subgraph path."
  [paths]
  (mapv (fn [edges]
          (vec (mapcat (fn [e]
                         (map (fn [[a b]] [a b]) e))
                       (butlast edges))))
        paths))
