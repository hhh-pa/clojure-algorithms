(ns clojure-algorithms.graphs.eulerian-path-and-circuit-for-undirected-graph
  "Eulerian path/circuit detection and traversal for undirected graphs.")

(defn- edge-key [u v] (if (< u v) [u v] [v u]))

(defn dfs
  "Returns an Eulerian traversal from u, sharing visited edges."
  ([u graph visited]
   (let [seen (atom (set visited))]
     (letfn [(visit [x path]
               (let [path (conj path x)]
                 (reduce (fn [p v]
                           (let [k (edge-key x v)]
                             (if (contains? @seen k)
                               p
                               (do (swap! seen conj k)
                                   (visit v p)))))
                         path (get graph x []))))]
       (visit u [])))))

(defn check-circuit-or-path
  "Returns [1 -1] for a circuit, [2 odd-node] for a path, [3 odd-node] else."
  [graph max-node]
  (let [[odd-count odd-node]
        (reduce (fn [[c o] i]
                  (if (and (contains? graph i)
                           (odd? (count (get graph i))))
                    [(inc c) i] [c o]))
                [0 -1] (range max-node))]
    (cond (zero? odd-count) [1 odd-node]
          (= odd-count 2) [2 odd-node]
          :else [3 odd-node])))

(defn check-euler
  "Returns {:type :cycle/:path/:none :path [...]} for the graph."
  [graph max-node]
  (let [[check odd-node] (check-circuit-or-path graph max-node)]
    (if (= check 3)
      {:type :none :path []}
      (let [start (if (= check 2) odd-node 1)]
        {:type (if (= check 2) :path :cycle)
         :path (dfs start graph #{})}))))
