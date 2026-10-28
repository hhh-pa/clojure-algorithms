(ns clojure-algorithms.graphs.bidirectional-breadth-first-search
  "Uni- and bidirectional breadth-first grid search.
   This port tracks visited cells (the Python original re-enqueues them,
   which is much slower); returned paths are still shortest.")

(def delta "Moves: up, left, down, right." [[-1 0] [0 -1] [1 0] [0 1]])

(defn- free-neighbours [grid [y x]]
  (into []
        (comp (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
              (filter (fn [[ny nx]]
                        (and (<= 0 nx (dec (count (first grid))))
                             (<= 0 ny (dec (count grid)))
                             (zero? (get-in grid [ny nx]))))))
        delta))

(defn- retrace [parents node]
  (loop [n node path (list node)]
    (if-let [p (get parents n)]
      (recur p (cons p path))
      (vec path))))

(defn bfs-search
  "Returns the shortest start->goal path, or [start] when unreachable."
  [grid start goal]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{start}
         parents {}]
    (if (empty? queue)
      [start]
      (let [v (peek queue)]
        (if (= v goal)
          (retrace parents v)
          (let [fresh (remove visited (free-neighbours grid v))]
            (recur (into (pop queue) fresh)
                   (into visited fresh)
                   (into parents (map #(vector % v) fresh)))))))))

(defn bidirectional-search
  "Returns the shortest start->goal path via two-sided BFS,
   or [start] when unreachable."
  [grid start goal]
  (if (= start goal)
    [start]
    (let [q (clojure.lang.PersistentQueue/EMPTY)]
      (loop [fq (conj q start) bq (conj q goal)
             fp {start nil} bp {goal nil}
             fv #{start} bv #{goal}]
        (cond
          (or (empty? fq) (empty? bq)) [start]
          :else
          (let [v (peek fq)
                hit (first (filter bv (free-neighbours grid v)))]
            (if (or (contains? bv v) hit)
              (let [meet (or (when (contains? bv v) v) hit)
                    fp (if hit (assoc fp hit v) fp)
                    fwd (retrace (dissoc fp start) meet)
                    bwd-path (loop [n (get bp meet) acc []]
                               (if (nil? n) acc
                                   (recur (get bp n) (conj acc n))))]
                (vec (concat fwd bwd-path)))
              (let [fresh (remove fv (free-neighbours grid v))
                    fq (into (pop fq) fresh)
                    fv (into fv fresh)
                    fp (into fp (map #(vector % v) fresh))
                    w (peek bq)
                    hit2 (first (filter fv (free-neighbours grid w)))]
                (if (or (contains? fv w) hit2)
                  (let [meet (or (when (contains? fv w) w) hit2)
                        bp (if hit2 (assoc bp hit2 w) bp)
                        fwd (retrace (dissoc fp start) meet)
                        bwd-path (loop [n (get bp meet) acc []]
                                   (if (nil? n) acc
                                       (recur (get bp n) (conj acc n))))]
                    (vec (concat fwd bwd-path)))
                  (let [fresh2 (remove bv (free-neighbours grid w))]
                    (recur fq (into (pop bq) fresh2)
                           fp (into bp (map #(vector % w) fresh2))
                           fv (into bv fresh2))))))))))))
