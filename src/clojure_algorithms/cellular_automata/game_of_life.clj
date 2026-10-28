(ns clojure-algorithms.cellular-automata.game-of-life
  "Conway's Game of Life over boolean grids (numpy/matplotlib omitted).")

(defn create-canvas
  "Creates a size x size grid of dead (false) cells."
  [size]
  (vec (repeat size (vec (repeat size false)))))

(defn seed
  "Returns a copy of canvas with each cell randomly alive or dead."
  [canvas]
  (mapv (fn [row] (mapv (fn [_] (rand-nth [true false])) row)) canvas))

(defn judge-point
  "Applies the Game of Life rules to cell pt with its 3x3 neighbours."
  [pt neighbours]
  (let [flat (apply concat neighbours)
        alive (count (filter true? flat))
        alive (if pt (dec alive) alive)]
    (if pt
      (cond (< alive 2) false (#{2 3} alive) true :else false)
      (= alive 3))))

(defn run
  "Computes the next generation with wrap-around edges."
  [canvas]
  (let [n (count canvas)]
    (if (zero? n)
      []
      (vec (map-indexed
            (fn [r row]
              (vec (map-indexed
                    (fn [c pt]
                      (judge-point
                       pt
                       (for [dr [-1 0 1]]
                         (for [dc [-1 0 1]]
                           (get-in canvas [(mod (+ r dr) n)
                                           (mod (+ c dc) n)])))))
                    row)))
            canvas)))))
