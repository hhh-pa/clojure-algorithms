(ns clojure-algorithms.graphs.bidirectional-a-star
  "Uni- and bidirectional A* grid search with Euclidean heuristic.
   Open/closed membership is by position (the Python original compares
   node identity, keeping duplicates); paths are still optimal.")

(def delta "Moves: up, left, down, right." [[-1 0] [0 -1] [1 0] [0 1]])

(defn- heuristic [[y x] [gy gx]]
  (Math/sqrt (+ (* (- x gx) (- x gx)) (* (- y gy) (- y gy)))))

(defn- make-node [pos goal g parent]
  {:pos pos :g g :parent parent :f (+ g (heuristic pos goal))})

(defn- successors [grid goal parent]
  (let [[y x] (:pos parent)]
    (into []
          (comp (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
                (filter (fn [[ny nx]]
                          (and (<= 0 nx (dec (count (first grid))))
                               (<= 0 ny (dec (count grid)))
                               (zero? (get-in grid [ny nx])))))
                (map #(make-node % goal (inc (:g parent)) parent)))
          delta)))

(defn- retrace [node]
  (loop [n node path (list (:pos node))]
    (if-let [p (:parent n)]
      (recur p (cons (:pos p) path))
      (vec path))))

(defn- first-min-by
  "Element with minimal (k %), keeping the first on ties."
  [k coll]
  (reduce (fn [a b] (if (< (k b) (k a)) b a)) coll))

(defn- expand [grid goal current open closed]
  (reduce (fn [open kid]
            (cond
              (contains? closed (:pos kid)) open
              (not (contains? open (:pos kid)))
              (assoc open (:pos kid) kid)
              (< (:g kid) (:g (get open (:pos kid))))
              (assoc open (:pos kid) kid)
              :else open))
          open (successors grid goal current)))

(defn astar-search
  "Returns the optimal start->goal path, or [start] when unreachable."
  [grid start goal]
  (loop [open {start (make-node start goal 0 nil)} closed #{}]
    (if (empty? open)
      [start]
      (let [current (first-min-by :f (vals open))
            open (dissoc open (:pos current))]
        (if (= (:pos current) goal)
          (retrace current)
          (recur (expand grid goal current open closed)
                 (conj closed (:pos current))))))))

(defn bidirectional-search
  "Returns the start->goal path via two-sided A*, or [start]."
  [grid start goal]
  (let [chain (fn [par from to]
                (loop [n to acc (list to)]
                  (if (= n from)
                    (vec acc)
                    (recur (get par n) (cons (get par n) acc)))))
        stitch (fn [f-par b-par meet]
                 (let [fwd (chain f-par start meet)
                       bwd (loop [n meet acc []]
                             (if (= n goal)
                               (vec acc)
                               (recur (get b-par n) (conj acc (get b-par n)))))]
                   (vec (concat fwd bwd))))]
    (loop [f-open {start (make-node start goal 0 nil)}
           b-open {goal (make-node goal start 0 nil)}
           f-closed #{} b-closed #{}
           f-par {start nil} b-par {goal nil}
           f-target goal b-target start]
      (if (or (empty? f-open) (empty? b-open))
        [start]
        (let [cf (first-min-by :f (vals f-open))
              cb (first-min-by :f (vals b-open))
              f-open (dissoc f-open (:pos cf))
              b-open (dissoc b-open (:pos cb))]
          (cond
            (or (contains? b-open (:pos cf)) (contains? b-closed (:pos cf)))
            (stitch f-par b-par (:pos cf))
            (or (contains? f-open (:pos cb)) (contains? f-closed (:pos cb)))
            (stitch f-par b-par (:pos cb))
            :else
            (let [f-closed (conj f-closed (:pos cf))
                  b-closed (conj b-closed (:pos cb))
                  f-kids (remove #(or (contains? f-closed (:pos %))
                                      (contains? f-open (:pos %)))
                                 (successors grid b-target cf))
                  b-kids (remove #(or (contains? b-closed (:pos %))
                                      (contains? b-open (:pos %)))
                                 (successors grid f-target cb))
                  f-meet (first (filter #(or (contains? b-open (:pos %))
                                             (contains? b-closed (:pos %)))
                                        f-kids))
                  b-meet (first (filter #(or (contains? f-open (:pos %))
                                             (contains? f-closed (:pos %)))
                                        b-kids))]
              (cond
                f-meet
                (stitch (assoc f-par (:pos f-meet) (:pos cf))
                        b-par (:pos f-meet))
                b-meet
                (stitch f-par
                        (assoc b-par (:pos b-meet) (:pos cb))
                        (:pos b-meet))
                :else
                (recur (into f-open (map (fn [k] [(:pos k) k]) f-kids))
                       (into b-open (map (fn [k] [(:pos k) k]) b-kids))
                       f-closed b-closed
                       (into f-par (map #(vector (:pos %) (:pos cf)) f-kids))
                       (into b-par (map #(vector (:pos %) (:pos cb)) b-kids))
                       (:pos cb) (:pos cf))))))))))
