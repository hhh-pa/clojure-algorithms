(ns clojure-algorithms.graphs.boruvka
  "Minimum spanning tree via Boruvka's algorithm.")

(defn make-graph
  "Builds an empty graph with num-nodes nodes."
  [num-nodes]
  {:num-nodes num-nodes :edges []})

(defn add-edge
  "Returns the graph with edge [u v weight] added."
  [graph u v weight]
  (update graph :edges conj [u v weight]))

(defn- find-root [component u]
  (loop [u u]
    (let [p (get component u)]
      (if (= p u) u (recur p)))))

(defn boruvka
  "Returns {:edges :weight} of the minimum spanning forest."
  [{:keys [num-nodes edges]}]
  (let [component (atom (into {} (map #(vector % %) (range num-nodes))))
        size (atom (vec (repeat num-nodes 1)))
        mst (atom [])
        weight (atom 0)
        ncomp (atom num-nodes)]
    (letfn [(flatten! [u]
              (when (not= (get @component u) u)
                (doseq [k (keys @component)]
                  (swap! component assoc k (find-root @component k)))))
            (union! [a b]
              (let [sa (nth @size a) sb (nth @size b)]
                (if (<= sa sb)
                  (do (swap! component assoc a b)
                      (swap! size assoc b (+ sb sa))
                      (flatten! a))
                  (do (swap! component assoc b (find-root @component a))
                      (swap! size assoc a (+ sa sb))
                      (flatten! b)))))]
      (while (> @ncomp 1)
        (let [best (atom (vec (repeat num-nodes nil)))]
          (doseq [[u v w] edges]
            (let [cu (get @component u) cv (get @component v)]
              (when (not= cu cv)
                (doseq [c [cu cv]]
                  (when (or (nil? (nth @best c)) (> (nth (nth @best c) 2) w))
                    (swap! best assoc c [u v w]))))))
          (doseq [edge @best]
            (when edge
              (let [[u v w] edge
                    cu (get @component u) cv (get @component v)]
                (when (not= cu cv)
                  (swap! weight + w)
                  (swap! mst conj [u v w])
                  (union! cu cv)
                  (swap! ncomp dec)))))))
      {:edges @mst :weight @weight})))
