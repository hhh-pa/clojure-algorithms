(ns clojure-algorithms.machine-learning.astar
  "A* best-first search on a grid world: f(n) = g(n) + h(n).

  Cells and worlds are immutable maps. The upstream numpy uses (a zero grid
  and an argmin over open-node costs) are plain operations here, so the
  whole module is genuinely portable.

  Reference: https://en.wikipedia.org/wiki/A*_search_algorithm")

(defn make-Cell
  "Cell at [0 0] with no parent and zero costs."
  []
  {:position [0 0] :parent nil :g 0 :h 0 :f 0})

(defn Cell-eq
  "Cells are equal when their positions match."
  [a b]
  (= (:position a) (:position b)))

(defn Cell-showcell
  "Print the cell position."
  [cell]
  (println (:position cell)))

(defn make-Gridworld
  "Grid world of all-zero rows with x/y limits (default 5x5)."
  ([] (make-Gridworld [5 5]))
  ([[wx wy]]
   {:w (vec (repeat wx (vec (repeat wy 0))))
    :world-x-limit wx
    :world-y-limit wy}))

(defn Gridworld-show
  "Print the world grid."
  [world]
  (println (:w world)))

(def ^:private neighbour-deltas
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn Gridworld-get-neighbours
  "In-bounds neighbouring cells of cell, each parented at cell."
  [world cell]
  (let [[cx cy] (:position cell)]
    (vec (keep (fn [[dx dy]]
                 (let [x (+ cx dx)
                       y (+ cy dy)]
                   (when (and (<= 0 x (dec (:world-x-limit world)))
                              (<= 0 y (dec (:world-y-limit world))))
                     (assoc (make-Cell) :position [x y] :parent cell))))
               neighbour-deltas))))

(defn- argmin-first
  [xs]
  (first (keep-indexed (fn [i v] (when (= v (apply min xs)) i)) xs)))

(defn astar
  "Shortest path of positions from start to goal.

  NOTE: the upstream inner membership scans are no-ops (their `continue`
  only affects the inner loop), so every neighbour is appended; this port
  does the same directly.

  >>> (astar (make-Gridworld)
  ...        (assoc (make-Cell) :position [0 0])
  ...        (assoc (make-Cell) :position [4 4]))
  [[0 0] [1 1] [2 2] [3 3] [4 4]]"
  [world start goal]
  (let [[_ _ current]
        (loop [open [start]
               closed []
               current nil]
          (if (empty? open)
            [open closed current]
            (let [idx (argmin-first (mapv :f open))
                  current (nth open idx)
                  open (vec (concat (subvec open 0 idx) (subvec open (inc idx))))
                  closed (conj closed current)]
              (if (Cell-eq current goal)
                [open closed current]
                (let [[gx gy] (:position goal)]
                  (recur (into open
                               (map (fn [n]
                                      (let [[x y] (:position n)
                                            g (inc (:g current))
                                            h (+ (* (- gy y) (- gy y))
                                                 (* (- gx x) (- gx x)))]
                                        (assoc n :g g :h h :f (+ h g))))
                                    (Gridworld-get-neighbours world current)))
                         closed
                         current))))))]
    (vec (reverse (loop [trail []
                         node current]
                    (if (nil? (:parent node))
                      (conj trail (:position node))
                      (recur (conj trail (:position node))
                             (:parent node))))))))
