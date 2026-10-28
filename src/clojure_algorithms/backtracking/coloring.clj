(ns clojure-algorithms.backtracking.coloring
  "Graph coloring (m-coloring problem): assign at most m colors to the
  vertices of an adjacency-matrix graph so no adjacent vertices share one.
  See https://en.wikipedia.org/wiki/Graph_coloring")

(defn valid-coloring?
  "Returns true when `color` can be assigned to a vertex with adjacency
  row `neighbours` given current `colored-vertices` (-1 means uncolored)."
  [neighbours colored-vertices color]
  (not-any? (fn [[i neighbour]]
              (and (= 1 neighbour) (= color (nth colored-vertices i))))
            (map-indexed vector neighbours)))

(defn util-color
  "Backtracking search: tries to extend `colored-vertices` from `index`
  onward using `max-colors`. Returns the completed assignment vector,
  or nil when impossible. Mirrors the Python recursion order, so the
  first solution found matches the reference."
  [graph max-colors colored-vertices index]
  (if (= index (count graph))
    colored-vertices
    (some (fn [c]
            (when (valid-coloring? (nth graph index) colored-vertices c)
              (util-color graph max-colors (assoc colored-vertices index c)
                          (inc index))))
          (range max-colors))))

(defn color
  "Returns a valid color assignment for `graph` using at most `max-colors`
  colors, or [] when impossible."
  [graph max-colors]
  (or (util-color graph max-colors (vec (repeat (count graph) -1)) 0) []))
