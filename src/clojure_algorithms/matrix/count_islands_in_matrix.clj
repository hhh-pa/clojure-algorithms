(ns clojure-algorithms.matrix.count-islands-in-matrix
  "Counts islands (8-connected groups of set cells) in a boolean matrix.")

(defrecord Matrix [graph ROW COL])

(defn make-Matrix
  "Builds a Matrix from a 2D grid of booleans."
  [graph]
  (->Matrix graph (count graph) (count (first graph))))

(def ^:private neighbor-deltas
  "The 8 offsets surrounding a cell."
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn- land?
  "Mimics Python's `cell == 1`: true, 1 and 1.0 count as land."
  [cell]
  (or (true? cell) (and (number? cell) (== 1 cell))))

(defn- cell-set?
  "Mimics Python truthiness of a grid cell: numbers count when non-zero."
  [cell]
  (if (number? cell) (not (zero? cell)) (boolean cell)))

(defn Matrix-is-safe
  "True when (i, j) is inside the matrix, unvisited and land."
  [self i j visited]
  (and (<= 0 i (dec (:ROW self)))
       (<= 0 j (dec (:COL self)))
       (not (get-in visited [i j]))
       (land? (get-in (:graph self) [i j]))))

(defn Matrix-diffs
  "Marks the island containing (i, j) as visited.
  Returns the updated visited grid (upstream mutates it in place)."
  [self i j visited]
  (reduce (fn [v [di dj]]
            (let [ni (+ i di) nj (+ j dj)]
              (if (Matrix-is-safe self ni nj v)
                (Matrix-diffs self ni nj v)
                v)))
          (assoc-in visited [i j] true)
          neighbor-deltas))

(defn Matrix-count-islands
  "Counts the 8-connected islands of set cells in the matrix."
  [self]
  (let [rows (:ROW self) cols (:COL self)]
    (loop [i 0
           j 0
           visited (vec (repeat rows (vec (repeat cols false))))
           n 0]
      (cond (>= i rows) n
            (>= j cols) (recur (inc i) 0 visited n)
            (and (not (get-in visited [i j]))
                 (cell-set? (get-in (:graph self) [i j])))
            (recur i (inc j) (Matrix-diffs self i j visited) (inc n))
            :else (recur i (inc j) visited n)))))
