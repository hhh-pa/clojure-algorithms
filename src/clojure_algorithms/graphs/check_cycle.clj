(ns clojure-algorithms.graphs.check-cycle
  "Directed-graph cycle detection via DFS with a recursion stack.")

(defn depth-first-search
  "True when a cycle is reachable from vertex, tracking visited/rec-stack."
  [graph vertex visited rec-stk]
  (let [visited (conj visited vertex)
        rec-stk (conj rec-stk vertex)]
    (loop [neighbours (get graph vertex []) visited visited]
      (if (empty? neighbours)
        [false visited]
        (let [n (first neighbours)]
          (cond
            (contains? rec-stk n) [true visited]
            (contains? visited n) (recur (rest neighbours) visited)
            :else (let [[found visited]
                        (depth-first-search graph n visited rec-stk)]
                    (if found [true visited]
                        (recur (rest neighbours) visited)))))))))

(defn check-cycle
  "True when the directed graph contains any cycle."
  [graph]
  (loop [vertices (keys graph) visited #{}]
    (if (empty? vertices)
      false
      (let [v (first vertices)]
        (if (contains? visited v)
          (recur (rest vertices) visited)
          (let [[found visited] (depth-first-search graph v visited #{})]
            (if found true (recur (rest vertices) visited))))))))
