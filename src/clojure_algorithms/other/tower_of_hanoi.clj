(ns clojure-algorithms.other.tower-of-hanoi
  "Tower of Hanoi move generation.

  Port of TheAlgorithms/Python `other/tower_of_hanoi.py`.
  `moves` returns the move list purely; `move-tower` prints it like
  the original. The stdin-driven `main` is not ported.")

(defn moves
  "Vector of `[from to]` moves transferring `height` disks from
  `from-pole` to `to-pole` using `with-pole`."
  [height from-pole to-pole with-pole]
  (if (< height 1)
    []
    (vec (concat (moves (dec height) from-pole with-pole to-pole)
                 [[from-pole to-pole]]
                 (moves (dec height) with-pole to-pole from-pole)))))

(defn move-tower
  "Print the moves, mirroring the Python original."
  [height from-pole to-pole with-pole]
  (doseq [[fp tp] (moves height from-pole to-pole with-pole)]
    (println "moving disk from" fp "to" tp)))
