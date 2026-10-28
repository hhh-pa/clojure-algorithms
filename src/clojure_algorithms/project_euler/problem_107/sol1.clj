(ns clojure-algorithms.project-euler.problem-107.sol1
  "Project Euler 107: minimal network.

  Find the maximum saving which can be achieved by removing redundant
  edges whilst ensuring that the network remains connected. Uses Prim's
  algorithm to find a minimum spanning tree.

  A graph is a map with :vertices (a set of ints) and :edges (a map
  from sorted [u v] edge vectors to weights).

  This port takes the network as a string (comma-separated adjacency
  matrix rows, '-' for no edge) instead of reading p107_network.txt, so
  the pure logic can be tested with tiny data."
  (:require [clojure.string :as str]))

(defn make-graph
  "Creates an undirected weighted graph from a set of vertices and a map
  of edges to weights, normalizing each edge to sorted order."
  [vertices edges]
  {:vertices (set vertices)
   :edges (into {}
                (map (fn [[edge weight]]
                       [[(min (first edge) (second edge))
                         (max (first edge) (second edge))]
                        weight]))
                edges)})

(defn add-edge
  "Adds a new edge with the given weight to the graph, returning the
  updated graph."
  [graph edge weight]
  (-> graph
      (update :vertices conj (first edge) (second edge))
      (assoc-in [:edges [(min (first edge) (second edge))
                         (max (first edge) (second edge))]]
                weight)))

(defn prims-algorithm
  "Runs Prim's algorithm to find the minimum spanning tree of the graph."
  [graph]
  (loop [subgraph (make-graph #{(apply min (:vertices graph))} {})]
    (if (= (count (:vertices subgraph)) (count (:vertices graph)))
      subgraph
      (let [[min-edge min-weight]
            (reduce (fn [[best-edge best-weight] [edge weight]]
                      (let [in-first (contains? (:vertices subgraph)
                                                (first edge))
                            in-second (contains? (:vertices subgraph)
                                                 (second edge))]
                        (if (and (or (and in-first (not in-second))
                                     (and in-second (not in-first)))
                                 (< weight best-weight))
                          [edge weight]
                          [best-edge best-weight])))
                    [nil (inc (apply max (vals (:edges graph))))]
                    (:edges graph))]
        (recur (add-edge subgraph min-edge min-weight))))))

(defn solution
  "Finds the maximum saving which can be achieved by removing redundant
  edges from the network in content whilst ensuring that the network
  remains connected."
  [content]
  (let [matrix (mapv #(str/split % #",")
                     (str/split-lines (str/trim content)))
        n (count matrix)
        edges (into {}
                    (for [edge1 (range 1 n)
                          edge2 (range edge1)
                          :let [cell (nth (nth matrix edge1) edge2)]
                          :when (not= "-" cell)]
                      [[edge2 edge1] (parse-long cell)]))
        graph (make-graph (set (range n)) edges)
        subgraph (prims-algorithm graph)
        initial-total (reduce + (vals (:edges graph)))
        optimal-total (reduce + (vals (:edges subgraph)))]
    (- initial-total optimal-total)))
