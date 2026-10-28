(ns clojure-algorithms.graphs.a-star
  "A* grid path search avoiding obstacles.")

(def directions "Movement deltas: left, down, right, up." [[-1 0] [0 -1] [1 0] [0 1]])

(defn search
  "Returns [path action-grid] from init to goal over grid (1 = obstacle).
   heuristic gives the estimated cost-to-go per cell. Throws when no path."
  [grid init goal cost heuristic]
  (let [rows (count grid)
        cols (count (first grid))
        closed (atom (vec (repeat rows (vec (repeat cols 0)))))
        action (atom (vec (repeat rows (vec (repeat cols 0)))))]
    (swap! closed assoc-in init 1)
    (let [[gx gy]
          (loop [cell [[(+ (get-in heuristic init)) 0
                        (first init) (second init)]]]
            (if (empty? cell)
              (throw (IllegalArgumentException.
                      "Algorithm is unable to find solution"))
              (let [sorted-cell (sort cell)
                    [_ g x y] (first sorted-cell)
                    rest-cell (vec (rest sorted-cell))]
                (if (and (= x (first goal)) (= y (second goal)))
                  [x y]
                  (let [fresh
                        (into []
                              (comp
                               (map-indexed
                                (fn [i [dx dy]]
                                  [i (+ x dx) (+ y dy)]))
                               (filter (fn [[_ x2 y2]]
                                         (and (<= 0 x2 (dec rows))
                                              (<= 0 y2 (dec cols))
                                              (zero? (get-in @closed [x2 y2]))
                                              (zero? (get-in grid [x2 y2]))))))
                              directions)]
                    (doseq [[i x2 y2] fresh]
                      (swap! closed assoc-in [x2 y2] 1)
                      (swap! action assoc-in [x2 y2] i))
                    (recur
                     (into rest-cell
                           (map (fn [[_ x2 y2]]
                                  [(+ g cost (get-in heuristic [x2 y2]))
                                   (+ g cost) x2 y2])
                                fresh))))))))]
      (let [path (loop [x gx y gy inv [[gx gy]]]
                   (if (and (= x (first init)) (= y (second init)))
                     (vec (reverse inv))
                     (let [[dx dy] (nth directions (get-in @action [x y]))
                           x2 (- x dx) y2 (- y dy)]
                       (recur x2 y2 (conj inv [x2 y2])))))]
        [path @action]))))
