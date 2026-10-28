(ns clojure-algorithms.dynamic-programming.floyd-warshall
  "All-pairs shortest paths via Floyd-Warshall.

  A graph is {:n n :dp matrix}; functions return updated graphs.
  Ported from TheAlgorithms/Python dynamic_programming/floyd_warshall.py.")

(def infinity
  "Representation of no path."
  Double/POSITIVE_INFINITY)

(defn make-graph
  "Graph with `n` nodes (0..n-1) and no edges."
  [n]
  {:n n :dp (vec (repeat n (vec (repeat n infinity))))})

(defn add-edge
  "Graph with directed edge u -> v of weight `w` added."
  [graph u v w]
  (assoc-in graph [:dp u v] w))

(defn floyd-warshall
  "Graph with :dp replaced by all-pairs shortest distances."
  [{:keys [n dp] :as graph}]
  (assoc graph :dp
         (reduce (fn [d k]
                   (reduce (fn [d2 i]
                             (reduce (fn [d3 j]
                                       (update-in d3 [i j] min
                                                  (+ (get-in d3 [i k])
                                                     (get-in d3 [k j]))))
                                     d2 (range n)))
                           d (range n)))
                 dp (range n))))

(defn show-min
  "Shortest distance from `u` to `v` (infinity when unreachable)."
  [graph u v]
  (get-in (:dp graph) [u v]))
