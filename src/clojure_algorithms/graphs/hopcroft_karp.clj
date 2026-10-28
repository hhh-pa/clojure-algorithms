(ns clojure-algorithms.graphs.hopcroft-karp
  "Maximum bipartite matching via Hopcroft-Karp.")

(defn- check-partitions [graph]
  (let [left (vec (keys graph))
        right (vec (distinct (apply concat (vals graph))))]
    (when (some nil? (concat left right))
      (throw (IllegalArgumentException. "Vertices cannot be None")))
    (let [overlap (filter (set right) left)]
      (when (seq overlap)
        (throw (IllegalArgumentException.
                (str "Partitions must be disjoint: found vertices in both "
                     "sets: " (vec (sort-by str overlap)))))))
    [left right]))

(defn hopcroft-karp
  "Returns {left-vertex right-vertex} maximum matching."
  [graph]
  (let [[left _] (check-partitions graph)
        pair-left (atom (into {} (map #(vector % nil) left)))
        pair-right (atom {})
        dist (atom {})]
    (letfn [(bfs []
              (let [q (java.util.ArrayDeque.)]
                (doseq [u left]
                  (if (nil? (get @pair-left u))
                    (do (swap! dist assoc u 0.0) (.add q u))
                    (swap! dist assoc u Double/POSITIVE_INFINITY)))
                (swap! dist assoc :nil Double/POSITIVE_INFINITY)
                (while (not (.isEmpty q))
                  (let [u (.poll q)]
                    (when (< (get @dist u) (get @dist :nil))
                      (doseq [v (get graph u [])]
                        (let [mu (get @pair-right v :nil)
                              key (if (= mu :nil) :nil mu)]
                          (when (= (get @dist key Double/POSITIVE_INFINITY)
                                   Double/POSITIVE_INFINITY)
                            (swap! dist assoc key (inc (get @dist u)))
                            (when (not= key :nil)
                              (.add q mu))))))))
                (not= (get @dist :nil) Double/POSITIVE_INFINITY)))
            (dfs [start]
              (loop [stack [start] indices [0] path []]
                (if (empty? stack)
                  false
                  (let [u (peek stack)
                        idx (peek indices)
                        neighbours (get graph u [])
                        on-layer? (fn [v]
                                    (let [mu (get @pair-right v :nil)
                                          ky (if (= mu :nil) :nil mu)]
                                      (= (get @dist ky Double/POSITIVE_INFINITY)
                                         (inc (get @dist u)))))
                        hit (loop [i idx]
                              (cond (>= i (count neighbours)) nil
                                    (on-layer? (nth neighbours i)) i
                                    :else (recur (inc i))))]
                    (if (nil? hit)
                      (do (swap! dist assoc u Double/POSITIVE_INFINITY)
                          (recur (pop stack) (pop indices)
                                 (if (seq path) (pop path) [])))
                      (let [v (nth neighbours hit)
                            mu (get @pair-right v :nil)
                            ky (if (= mu :nil) :nil mu)
                            indices (assoc indices (dec (count indices))
                                           (inc hit))
                            path (conj path [u v])]
                        (if (= ky :nil)
                          (do (doseq [[pl pr] path]
                                (swap! pair-right assoc pr pl)
                                (swap! pair-left assoc pl pr))
                              true)
                          (recur (conj stack mu) (conj indices 0)
                                 path))))))))]
      (while (bfs)
        (doseq [u left]
          (when (nil? (get @pair-left u))
            (dfs u))))
      (into {} (filter (fn [[_ v]] (some? v)) @pair-left)))))
