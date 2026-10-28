(ns clojure-algorithms.sorts.topological-sort
  "Topological sort over a small example DAG.
  Ported from TheAlgorithms/Python.")

(def edges
  "Adjacency lists of the example DAG."
  {"a" ["c" "b"]
   "b" ["d" "e"]
   "c" []
   "d" []
   "e" []})

(def vertices
  "All vertices of the example DAG."
  ["a" "b" "c" "d" "e"])

(defn topological-sort
  "Depth-first topological sort from `start`, threading `visited` and
  `sorted` vectors. Returns the bottom-up order vector.
  Throws IllegalArgumentException unless both are vectors."
  [start visited sorted]
  (when-not (vector? visited)
    (throw (IllegalArgumentException. "visited must be a list")))
  (when-not (vector? sorted)
    (throw (IllegalArgumentException. "sort must be a list")))
  (let [[visited2 sorted2]
        (letfn [(visit [[vis acc] node]
                  (if (some #{node} vis)
                    [vis acc]
                    (let [[vis2 acc2]
                          (reduce visit [(conj vis node) acc]
                                  (get edges node []))]
                      [vis2 (conj acc2 node)])))]
          (visit [visited sorted] start))
        [visited3 sorted3]
        (reduce (fn [[vis acc] vtx]
                  (if (some #{vtx} vis)
                    [vis acc]
                    (letfn [(visit2 [[vis2 acc2] node]
                              (if (some #{node} vis2)
                                [vis2 acc2]
                                (let [[vis3 acc3]
                                      (reduce visit2 [(conj vis2 node) acc2]
                                              (get edges node []))]
                                  [vis3 (conj acc3 node)])))]
                      (visit2 [vis acc] vtx))))
                [visited2 sorted2] vertices)]
    sorted3))
