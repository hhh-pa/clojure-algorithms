(ns clojure-algorithms.backtracking.hamiltonian-cycle
  "Hamiltonian cycle: a graph cycle visiting each node exactly once
  (NP-complete).
  See https://en.wikipedia.org/wiki/Hamiltonian_path")

(defn valid-connection?
  "Returns true when `next-ver` can follow the vertex at `curr-ind - 1` in
  `path`: an edge must exist and the vertex must not be in the path yet."
  [graph next-ver curr-ind path]
  (and (= 1 (get-in graph [(nth path (dec curr-ind)) next-ver]))
       (not (some #(= next-ver %) path))))

(defn util-hamilton-cycle
  "Backtracking search filling `path` from `curr-ind` onward. Returns the
  completed path vector, or nil when no cycle extends it."
  [graph path curr-ind]
  (if (= curr-ind (count graph))
    (when (= 1 (get-in graph [(nth path (dec curr-ind)) (nth path 0)]))
      path)
    (some (fn [next-ver]
            (when (valid-connection? graph next-ver curr-ind path)
              (util-hamilton-cycle graph (assoc path curr-ind next-ver)
                                   (inc curr-ind))))
          (range (count graph)))))

(defn hamilton-cycle
  "Returns a Hamiltonian cycle of adjacency-matrix `graph` starting at
  `start-index` (first vertex repeated at the end), or [] when none exists."
  ([graph] (hamilton-cycle graph 0))
  ([graph start-index]
   (let [path (assoc (vec (repeat (inc (count graph)) -1)) 0 start-index
                     (count graph) start-index)]
     (or (util-hamilton-cycle graph path 1) []))))
