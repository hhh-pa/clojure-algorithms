(ns clojure-algorithms.graphs.graph-adjacency-list
  "Unweighted adjacency-list graph with strict vertex/edge management.")

(declare add-vertex add-edge)

(defn make-graph
  "Builds a graph from vertices and [src dst] edges (directed by default)."
  ([vertices edges] (make-graph vertices edges true))
  ([vertices edges directed]
   (reduce (fn [g [s d]] (add-edge g s d))
           (reduce add-vertex {:directed directed :adj {}} (or vertices []))
           (or edges []))))

(defn contains-vertex? [graph vertex]
  (contains? (:adj graph) vertex))

(defn- require-vertices [graph s d]
  (when-not (and (contains-vertex? graph s) (contains-vertex? graph d))
    (throw (IllegalArgumentException.
            (str "Incorrect input: Either " s " or " d " does not exist")))))

(defn contains-edge?
  "True when the s->d edge exists. Throws when a vertex is missing."
  [graph s d]
  (require-vertices graph s d)
  (boolean (some #{d} (get-in graph [:adj s]))))

(defn add-vertex
  "Returns the graph with vertex added. Throws when it already exists."
  [graph vertex]
  (when (contains-vertex? graph vertex)
    (throw (IllegalArgumentException.
            (str "Incorrect input: " vertex " is already in the graph."))))
  (assoc-in graph [:adj vertex] []))

(defn add-edge
  "Returns the graph with the s->d edge added."
  [graph s d]
  (require-vertices graph s d)
  (when (contains-edge? graph s d)
    (throw (IllegalArgumentException.
            (str "Incorrect input: The edge already exists between "
                 s " and " d))))
  (let [g (update-in graph [:adj s] conj d)]
    (if (:directed g)
      g (update-in g [:adj d] conj s))))

(defn remove-edge
  "Returns the graph without the s->d edge."
  [graph s d]
  (require-vertices graph s d)
  (when-not (contains-edge? graph s d)
    (throw (IllegalArgumentException.
            (str "Incorrect input: The edge does NOT exist between "
                 s " and " d))))
  (let [g (update-in graph [:adj s] (fn [vs] (vec (remove #{d} vs))))]
    (if (:directed g)
      g (update-in g [:adj d] (fn [vs] (vec (remove #{s} vs)))))))

(defn remove-vertex
  "Returns the graph without vertex and its incident edges."
  [graph vertex]
  (when-not (contains-vertex? graph vertex)
    (throw (IllegalArgumentException.
            (str "Incorrect input: " vertex " does not exist in this graph."))))
  (let [adj (dissoc (:adj graph) vertex)]
    (assoc graph :adj
           (into {} (map (fn [[k vs]] [k (vec (remove #{vertex} vs))])
                         adj)))))

(defn clear-graph
  "Returns the graph with all vertices and edges removed."
  [graph]
  (assoc graph :adj {}))
