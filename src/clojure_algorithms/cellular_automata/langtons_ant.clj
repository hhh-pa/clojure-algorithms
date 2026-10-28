(ns clojure-algorithms.cellular-automata.langtons-ant
  "Langton's ant. State is an immutable map; matplotlib display is omitted.")

(def width 80)
(def height 80)

(def ^:private directions {0 [-1 0] 1 [0 1] 2 [1 0] 3 [0 -1]})

(defn make-ant
  "Creates an ant on a w x h white board, facing left (270 degrees)."
  [w h]
  {:board (vec (repeat h (vec (repeat w true))))
   :pos [(quot w 2) (quot h 2)]
   :dir 3})

(defn move-ant
  "Advances one step: turn, move, flip the previous square."
  [{:keys [board pos dir] :as state}]
  (let [[x y] pos
        white (get-in board [x y])
        dir' (mod ((if white inc dec) dir) 4)
        [dx dy] (directions dir')]
    (-> state
        (assoc-in [:board x y] (not white))
        (assoc :pos [(+ x dx) (+ y dy)] :dir dir'))))

(defn simulate
  "Advances the ant n steps."
  [state n]
  (nth (iterate move-ant state) n))
