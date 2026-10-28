(ns clojure-algorithms.graphs.graph-list
  "Directed/undirected graph over an adjacency map.")

(defn make-graph
  "Builds an empty graph (directed by default)."
  ([] (make-graph true))
  ([directed] {:directed directed :adj {}}))

(defn add-edge
  "Returns the graph with the source->destination edge added."
  [{:keys [directed adj] :as graph} source destination]
  (let [adj (if directed
              (-> adj
                  (update source (fnil conj []) destination)
                  (update destination #(or % [])))
              (-> adj
                  (update source (fnil conj []) destination)
                  (update destination (fnil conj []) source)))]
    (assoc graph :adj adj)))
