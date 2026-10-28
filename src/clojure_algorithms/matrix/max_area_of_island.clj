(ns clojure-algorithms.matrix.max-area-of-island
  "Finds the maximum area of a 4-connected island of 1s in a binary grid.")

(def matrix
  "Example grid used by the doctests."
  [[0 0 1 0 0 0 0 1 0 0 0 0 0]
   [0 0 0 0 0 0 0 1 1 1 0 0 0]
   [0 1 1 0 1 0 0 0 0 0 0 0 0]
   [0 1 0 0 1 1 0 0 1 0 1 0 0]
   [0 1 0 0 1 1 0 0 1 1 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 1 1 1 0 0 0]
   [0 0 0 0 0 0 0 1 1 0 0 0 0]])

(defn is-safe
  "True when (row, col) lies inside a rows x cols grid."
  [row col rows cols]
  (and (<= 0 row (dec rows)) (<= 0 col (dec cols))))

(defn- explore
  "Flood-fills the island at (row, col), threading the seen set.
  Returns [area updated-seen]."
  [row col seen mat]
  (if (and (is-safe row col (count mat) (count (first mat)))
           (not (contains? seen [row col]))
           (= 1 (get-in mat [row col])))
    (let [seen (conj seen [row col])
          [a1 s1] (explore (inc row) col seen mat)
          [a2 s2] (explore (dec row) col s1 mat)
          [a3 s3] (explore row (inc col) s2 mat)
          [a4 s4] (explore row (dec col) s3 mat)]
      [(+ 1 a1 a2 a3 a4) s4])
    [0 seen]))

(defn depth-first-search
  "Returns the area of the island containing (row, col)."
  [row col seen mat]
  (first (explore row col (set seen) mat)))

(defn find-max-area
  "Returns the largest island area in the grid, or 0 when there is none."
  [mat]
  (second
   (reduce (fn [[seen best] [r row]]
             (reduce (fn [[seen best] [c v]]
                       (if (and (= 1 v) (not (contains? seen [r c])))
                         (let [[area seen] (explore r c seen mat)]
                           [seen (max best area)])
                         [seen best]))
                     [seen best]
                     (map-indexed vector row)))
           [#{} 0]
           (map-indexed vector mat))))
