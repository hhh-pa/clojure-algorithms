(ns clojure-algorithms.cellular-automata.conways-game-of-life
  "Conway's Game of Life. The PIL image renderer is replaced by a pure
  sequence of successive grids.")

(def glider
  [[0 1 0 0 0 0 0 0]
   [0 0 1 0 0 0 0 0]
   [1 1 1 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(def blinker [[0 1 0] [0 1 0] [0 1 0]])

(defn- live-neighbours
  [cells i j]
  (let [rows (count cells) cols (count (cells 0))]
    (count (for [di [-1 0 1] dj [-1 0 1]
                 :when (not (and (zero? di) (zero? dj)))
                 :let [ni (+ i di) nj (+ j dj)]
                 :when (and (<= 0 ni (dec rows)) (<= 0 nj (dec cols)))
                 :when (= 1 (get-in cells [ni nj]))]
             1))))

(defn new-generation
  "Computes the next generation of Conway's Game of Life grid."
  [cells]
  (vec (map-indexed
        (fn [i row]
          (vec (map-indexed
                (fn [j cell]
                  (let [n (live-neighbours cells i j) alive (= 1 cell)]
                    (if (or (and alive (<= 2 n 3))
                            (and (not alive) (= 3 n)))
                      1 0)))
                row)))
        cells)))

(defn generations
  "Lazily yields successive generations starting from cells."
  [cells]
  (iterate new-generation cells))
