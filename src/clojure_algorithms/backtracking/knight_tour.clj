(ns clojure-algorithms.backtracking.knight-tour
  "Open Knight's Tour: visit every square of an n x n board exactly once.
  Positions are [y x] vectors; the board is a vector of vectors where 0
  marks an unvisited square.")

(def ^:private knight-moves
  [[1 2] [-1 2] [1 -2] [-1 -2] [2 1] [2 -1] [-2 1] [-2 -1]])

(defn get-valid-pos
  "Returns all [y x] squares a knight can reach from `position` on an
  n x n board."
  [[y x] n]
  (vec (for [[dy dx] knight-moves
             :let [ny (+ y dy), nx (+ x dx)]
             :when (and (<= 0 ny (dec n)) (<= 0 nx (dec n)))]
         [ny nx])))

(defn complete?
  "Returns true when the board holds no zeros."
  [board]
  (not-any? zero? (apply concat board)))

(defn- tour-helper
  "DFS from `pos` with `curr` squares filled. Returns the completed board
  or nil."
  [board pos curr]
  (if (complete? board)
    board
    (some (fn [[y x :as position]]
            (when (zero? (get-in board [y x]))
              (tour-helper (assoc-in board [y x] (inc curr)) position (inc curr))))
          (get-valid-pos pos (count board)))))

(defn open-knight-tour
  "Returns a completed n x n tour board. Throws when no tour exists."
  [n]
  (or (some (fn [[i j]]
              (tour-helper (assoc-in (vec (repeat n (vec (repeat n 0))))
                                     [i j] 1)
                           [i j] 1))
            (for [i (range n) j (range n)] [i j]))
      (throw (ex-info (format "Open Knight Tour cannot be performed on a board of size %d" n)
                      {:n n}))))
