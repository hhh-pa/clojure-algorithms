(ns clojure-algorithms.graphs.prim
  "Minimum spanning tree via Prim's algorithm, plain and heap versions.")

(defn make-graph
  "Builds an n-vertex graph (adjacency {id {neighbour weight}})."
  [n]
  {:n n :adj (into {} (map #(vector % {}) (range n)))})

(defn connect
  "Connects 1-based vertices a<->b with weight (ignores out-of-range)."
  [{:keys [n] :as graph} a b weight]
  (let [a (dec a) b (dec b)]
    (if (or (< a 0) (< b 0) (>= a n) (>= b n) (= a b))
      graph
      (-> graph
          (assoc-in [:adj a b] weight)
          (assoc-in [:adj b a] weight)))))

(defn- prim* [adj root]
  (let [n (count adj)]
    (loop [heap (sorted-set [0 root])
           key (assoc (into {} (map #(vector % Long/MAX_VALUE)
                                    (keys adj))) root 0)
           pi {}
           done #{}]
      (if (empty? heap)
        (vec (for [i (sort (keys adj)) :when (not= i root)]
               [(inc i) (inc (get pi i))]))
        (let [[_ u] (first heap)
              heap (disj heap (first heap))]
          (if (contains? done u)
            (recur heap key pi done)
            (let [done (conj done u)
                  [heap key pi]
                  (reduce (fn [[h k p] [v w]]
                            (if (and (not (contains? done v))
                                     (< w (get k v)))
                              [(conj (if (= (get k v) Long/MAX_VALUE)
                                       h (disj h [(get k v) v]))
                                     [w v])
                               (assoc k v w) (assoc p v u)]
                              [h k p]))
                          [heap key pi] (get adj u))]
              (recur heap key pi done))))))))

(defn prim
  "Returns MST edges as 1-based [vertex parent] pairs."
  [{:keys [adj]} root]
  (prim* adj root))

(defn prim-heap
  "Heap-based Prim's; returns MST edges as 1-based pairs."
  [{:keys [adj]} root]
  (prim* adj root))
