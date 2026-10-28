(ns clojure-algorithms.graphs.random-graph-generator
  "Random and complete graph generation.")

(defn complete-graph
  "Returns the complete graph over vertices-number vertices."
  [vertices-number]
  (into {} (map (fn [i]
                  [i (vec (remove #(= % i) (range vertices-number)))])
                (range vertices-number))))

(defn random-graph
  "Returns a graph where each edge exists with probability."
  [vertices-number probability & {:keys [directed] :or {directed false}}]
  (cond
    (>= probability 1) (complete-graph vertices-number)
    (<= probability 0) (into {} (map #(vector % []) (range vertices-number)))
    :else
    (let [g (atom (into {} (map #(vector % []) (range vertices-number))))]
      (doseq [i (range vertices-number) j (range (inc i) vertices-number)]
        (when (< (Math/random) probability)
          (swap! g update i conj j)
          (when-not directed
            (swap! g update j conj i))))
      @g)))
