(ns clojure-algorithms.graphs.dijkstra-binary-grid
  "Dijkstra shortest path on a binary grid (1 walkable, 0 obstacle).")

(defn dijkstra
  "Returns [distance path] from source to destination. Distance is infinite
   with an empty path when unreachable."
  [grid source destination allow-diagonal]
  (let [rows (count grid)
        cols (count (first grid))
        deltas (if allow-diagonal
                 [[-1 0] [1 0] [0 -1] [0 1] [-1 -1] [-1 1] [1 -1] [1 1]]
                 [[-1 0] [1 0] [0 -1] [0 1]])]
    (loop [queue (sorted-set [0 source]) visited #{}
           dist {source 0} pred {source nil}]
      (if (empty? queue)
        [Double/POSITIVE_INFINITY []]
        (let [[d [x y :as cell]] (first queue)
              queue (disj queue [d cell])]
          (if (contains? visited cell)
            (recur queue visited dist pred)
            (let [visited (conj visited cell)]
              (if (= cell destination)
                [(double (get dist destination))
                 (loop [c destination path (list destination)]
                   (if (= c source)
                     (vec path)
                     (recur (get pred c) (cons (get pred c) path))))]
                (let [[queue dist pred]
                      (reduce (fn [[q ds ps] [dx dy]]
                                (let [nx (+ x dx) ny (+ y dy)]
                                  (if (and (<= 0 nx (dec rows))
                                           (<= 0 ny (dec cols))
                                           (= 1 (get-in grid [nx ny]))
                                           (> (get ds [nx ny]
                                                   Double/POSITIVE_INFINITY)
                                              (inc d)))
                                    [(conj q [(inc d) [nx ny]])
                                     (assoc ds [nx ny] (inc d))
                                     (assoc ps [nx ny] cell)]
                                    [q ds ps])))
                              [queue dist pred] deltas)]
                  (recur queue visited dist pred))))))))))
