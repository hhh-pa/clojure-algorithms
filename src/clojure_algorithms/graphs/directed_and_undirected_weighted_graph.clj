(ns clojure-algorithms.graphs.directed-and-undirected-weighted-graph
  "Weighted directed/undirected graphs: traversal, degrees, topo sort,
   cycle detection. Cycle routines use standard algorithms (Tarjan /
   leaf pruning); timing helpers measure the traversals.")

(defn make-graph
  "Builds an empty graph (directed by default)."
  ([] (make-graph true))
  ([directed] {:directed directed :adj {}}))

(defn- add-one [adj u v w]
  (let [cur (get adj u)]
    (if (seq cur)
      (if (some #(= % [w v]) cur)
        adj (assoc adj u (conj cur [w v])))
      (assoc adj u [[w v]]))))

(defn add-pair
  "Returns the graph with edge u->v (both ways if undirected)."
  ([graph u v] (add-pair graph u v 1))
  ([{:keys [directed] :as graph} u v w]
   (let [adj (add-one (:adj graph) u v w)
         adj (if (get adj v) adj (assoc adj v []))
         adj (if directed
               adj
               (let [adj (add-one adj v u w)]
                 (if (get adj u) adj (assoc adj u []))))]
     (assoc graph :adj adj))))

(defn remove-pair
  "Removes all u->v edges (both ways if undirected); silent if absent."
  [{:keys [directed] :as graph} u v]
  (let [strip (fn [adj a b]
                (if (seq (get adj a))
                  (assoc adj a (vec (remove #(= (second %) b)
                                            (get adj a))))
                  adj))
        adj (strip (:adj graph) u v)
        adj (if directed adj (strip adj v u))]
    (assoc graph :adj adj)))

(defn all-nodes [graph] (vec (keys (:adj graph))))

(defn- default-start [graph s]
  (if (nil? s)
    (first (keys (:adj graph)))
    s))

(defn dfs
  "Depth-first visit order from s, stopping after d when found."
  ([graph] (dfs graph nil nil))
  ([graph s] (dfs graph s nil))
  ([graph s d]
   (let [s (default-start graph s)]
     (if (nil? s)
       []
       (loop [stack [s] visited []]
         (if (empty? stack)
           visited
           (let [v (peek stack)
                 visited (conj visited v)]
             (if (= v d)
               visited
               (recur (into (pop stack)
                            (remove (set visited)
                                    (map second (get-in graph [:adj v]))))
                      visited)))))))))

(defn fill-graph-randomly
  "Adds c random 1-weighted edges (c random 10..10009 when omitted)."
  ([graph] (fill-graph-randomly
            graph (+ 10 (rand-int 10000))))
  ([graph c]
   (reduce (fn [g i]
             (reduce (fn [g _]
                       (let [n (inc (rand-int c))]
                         (if (= n i) g (add-pair g i n 1))))
                     g (range (inc (rand-int 102)))))
           graph (range c))))

(defn bfs
  "Breadth-first visit order from s."
  ([graph] (bfs graph nil))
  ([graph s]
   (let [s (default-start graph s)]
     (if (nil? s)
       []
       (loop [queue (conj clojure.lang.PersistentQueue/EMPTY s)
              visited [s]
              seen #{s}]
         (if (empty? queue)
           visited
           (let [v (peek queue)
                 fresh (remove seen
                               (map second (get-in graph [:adj v])))]
             (recur (into (pop queue) fresh)
                    (into visited fresh)
                    (into seen fresh)))))))))

(defn in-degree [graph u]
  (count (for [[_ ns] (:adj graph) [w v] ns :when (= v u)] w)))

(defn out-degree [graph u] (count (get-in graph [:adj u] [])))
(defn degree [graph u] (out-degree graph u))

(defn topological-sort
  "Iterative post-order topo sort from s (default first node)."
  ([graph] (topological-sort graph nil))
  ([graph s]
   (let [s (default-start graph s)]
     (loop [stack [s] visited #{s} order [] cur s]
       (if (empty? stack)
         (vec (reverse order))
         (let [nxt (first (remove visited
                                  (map second
                                       (get-in graph [:adj cur]))))]
           (if nxt
             (recur (conj stack nxt) (conj visited nxt) order nxt)
             (recur (pop stack) visited (conj order cur)
                    (peek (pop stack))))))))))

(defn- tarjan-scc [adj]
  (let [idx (atom {}) low (atom {}) on (atom #{}) stack (atom [])
        n (atom 0) out (atom [])]
    (letfn [(sc [v]
              (swap! idx assoc v @n) (swap! low assoc v @n) (swap! n inc)
              (swap! stack conj v) (swap! on conj v)
              (doseq [[_ w] (get adj v [])]
                (if (not (contains? @idx w))
                  (do (sc w)
                      (swap! low assoc v (min (get @low v) (get @low w))))
                  (when (contains? @on w)
                    (swap! low assoc v (min (get @low v) (get @idx w))))))
              (when (= (get @low v) (get @idx v))
                (let [comp (atom [])]
                  (loop []
                    (let [w (peek @stack)]
                      (swap! stack pop) (swap! on disj w)
                      (swap! comp conj w)
                      (when (not= w v) (recur))))
                  (swap! out conj @comp))))]
      (doseq [v (keys adj)]
        (when-not (contains? @idx v) (sc v))))
    @out))

(defn cycle-nodes
  "Nodes lying on any cycle (directed: nontrivial SCCs; undirected:
   survivors of iterative leaf pruning)."
  [{:keys [directed adj] :as graph}]
  (if directed
    (vec (mapcat (fn [comp]
                   (when (or (> (count comp) 1)
                             (let [v (first comp)]
                               (some #(= v (second %)) (get adj v []))))
                     comp))
                 (tarjan-scc adj)))
    (loop [deg (into {} (map (fn [[k v]] [k (count v)]) adj))]
      (let [leaves (filter #(<= (val %) 1) deg)]
        (if (empty? leaves)
          (vec (keys deg))
          (let [gone (set (keys (into {} leaves)))
                deg (apply dissoc deg gone)]
            (recur (into {}
                         (map (fn [[k vs]]
                                [k (count (remove (fn [[_ v]] (contains? gone v))
                                                  (get adj k)))])
                              deg)))))))))

(defn has-cycle?
  "True when the graph contains any cycle."
  [graph]
  (boolean (seq (cycle-nodes graph))))

(defn dfs-time
  "Seconds to run dfs."
  ([graph] (dfs-time graph nil nil))
  ([graph s] (dfs-time graph s nil))
  ([graph s e]
   (let [t0 (System/nanoTime)]
     (dfs graph s e)
     (/ (- (System/nanoTime) t0) 1e9))))

(defn bfs-time
  "Seconds to run bfs."
  ([graph] (bfs-time graph nil))
  ([graph s]
   (let [t0 (System/nanoTime)]
     (bfs graph s)
     (/ (- (System/nanoTime) t0) 1e9))))
