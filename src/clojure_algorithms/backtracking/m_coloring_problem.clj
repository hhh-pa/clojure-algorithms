(ns clojure-algorithms.backtracking.m-coloring-problem
  "M-coloring decision problem: can the vertices of an adjacency-matrix
  graph be colored with at most `max-colors` colors (numbered from 1)?")

(defn safe?
  "Returns true when `color` can be assigned to `node` given the current
  partial assignment `col` (0 means uncolored)."
  [node color graph num-vertices col]
  (every? (fn [k] (not (and (= 1 (get-in graph [node k]))
                            (= color (nth col k)))))
          (range num-vertices)))

(defn solve
  "Backtracking search from `node` onward. Returns true when the graph
  can be colored with colors 1..`max-colors`."
  [node col max-colors num-vertices graph]
  (if (= node num-vertices)
    true
    (boolean (some (fn [c]
                     (when (safe? node c graph num-vertices col)
                       (solve (inc node) (assoc col node c)
                              max-colors num-vertices graph)))
                   (range 1 (inc max-colors))))))

(defn graph-coloring
  "Returns true when `graph` (adjacency matrix over `num-vertices`
  vertices) admits a coloring with at most `max-colors` colors."
  [graph max-colors num-vertices]
  (solve 0 (vec (repeat num-vertices 0)) max-colors num-vertices graph))
