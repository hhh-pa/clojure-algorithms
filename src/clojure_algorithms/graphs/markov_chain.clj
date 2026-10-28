(ns clojure-algorithms.graphs.markov-chain
  "Random walks over a transition-probability graph.")

(defn make-graph
  "Builds an empty transition graph."
  [] {:connections {}})

(defn add-node [graph node]
  (if (contains? (:connections graph) node)
    graph (assoc-in graph [:connections node] {})))

(defn add-transition-probability
  "Sets the node1->node2 transition probability."
  [graph node1 node2 probability]
  (-> graph
      (add-node node1) (add-node node2)
      (assoc-in [:connections node1 node2] probability)))

(defn get-nodes [graph] (vec (keys (:connections graph))))

(defn transition
  "Takes one random step from node; returns \"\" when stuck."
  [graph node]
  (let [r (Math/random)]
    (loop [dests (seq (get-in graph [:connections node])) acc 0]
      (if (empty? dests)
        ""
        (let [[dest p] (first dests)
              acc (+ acc p)]
          (if (> acc r) dest (recur (rest dests) acc)))))))

(defn get-transitions
  "Walks steps transitions from start; returns {node visit-count}."
  [start transitions steps]
  (let [graph (reduce (fn [g [n1 n2 p]]
                        (add-transition-probability g n1 n2 p))
                      (make-graph) transitions)]
    (loop [i 0 node start
           visited (into {} (map #(vector % 1) (get-nodes graph)))]
      (if (>= i steps)
        visited
        (let [node (transition graph node)]
          (recur (inc i) node (update visited node (fnil inc 0))))))))
