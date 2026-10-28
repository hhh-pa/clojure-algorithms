(ns clojure-algorithms.graphs.graph-adjacency-matrix
  "Unweighted adjacency-matrix graph with strict vertex/edge management.")

(declare add-vertex add-edge)

(defn make-graph
  "Builds a graph from vertices and [src dst] edges (directed by default)."
  ([vertices edges] (make-graph vertices edges true))
  ([vertices edges directed]
   (reduce (fn [g [s d]] (add-edge g s d))
           (reduce add-vertex
                   {:directed directed :to-index {} :matrix []}
                   (or vertices []))
           (or edges []))))

(defn contains-vertex? [graph vertex]
  (contains? (:to-index graph) vertex))

(defn- require-vertices [graph s d]
  (when-not (and (contains-vertex? graph s) (contains-vertex? graph d))
    (throw (IllegalArgumentException.
            (str "Incorrect input: Either " s " or " d " does not exist")))))

(defn contains-edge?
  "True when the s->d edge exists. Throws when a vertex is missing."
  [graph s d]
  (require-vertices graph s d)
  (= 1 (get-in graph [:matrix (get-in graph [:to-index s])
                      (get-in graph [:to-index d])])))

(defn add-vertex
  "Returns the graph with vertex added. Throws when it already exists."
  [graph vertex]
  (when (contains-vertex? graph vertex)
    (throw (IllegalArgumentException.
            (str "Incorrect input: " vertex " already exists in this graph."))))
  (let [matrix (mapv #(conj % 0) (:matrix graph))
        matrix (conj matrix (vec (repeat (inc (count matrix)) 0)))]
    (-> graph
        (assoc :matrix matrix)
        (assoc-in [:to-index vertex] (dec (count matrix))))))

(defn add-edge
  "Returns the graph with the s->d edge added."
  [graph s d]
  (require-vertices graph s d)
  (when (contains-edge? graph s d)
    (throw (IllegalArgumentException.
            (str "Incorrect input: The edge already exists between "
                 s " and " d))))
  (let [u (get-in graph [:to-index s])
        v (get-in graph [:to-index d])
        g (assoc-in graph [:matrix u v] 1)]
    (if (:directed g) g (assoc-in g [:matrix v u] 1))))

(defn remove-edge
  "Returns the graph without the s->d edge."
  [graph s d]
  (require-vertices graph s d)
  (when-not (contains-edge? graph s d)
    (throw (IllegalArgumentException.
            (str "Incorrect input: The edge does NOT exist between "
                 s " and " d))))
  (let [u (get-in graph [:to-index s])
        v (get-in graph [:to-index d])
        g (assoc-in graph [:matrix u v] 0)]
    (if (:directed g) g (assoc-in g [:matrix v u] 0))))

(defn remove-vertex
  "Returns the graph without vertex and its incident edges."
  [graph vertex]
  (when-not (contains-vertex? graph vertex)
    (throw (IllegalArgumentException.
            (str "Incorrect input: " vertex " does not exist in this graph."))))
  (let [idx (get-in graph [:to-index vertex])
        matrix (vec (concat (subvec (:matrix graph) 0 idx)
                            (subvec (:matrix graph) (inc idx))))
        matrix (mapv #(vec (concat (subvec % 0 idx) (subvec % (inc idx))))
                     matrix)
        to-index (into {}
                       (map (fn [[v i]] [v (if (> i idx) (dec i) i)])
                            (dissoc (:to-index graph) vertex)))]
    (assoc graph :matrix matrix :to-index to-index)))

(defn clear-graph
  "Returns the graph with all vertices and edges removed."
  [graph]
  (assoc graph :to-index {} :matrix []))
