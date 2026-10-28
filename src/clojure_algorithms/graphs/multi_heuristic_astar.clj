(ns clojure-algorithms.graphs.multi-heuristic-astar
  "Multi-heuristic A* grid search (one consistent + two inadmissible).")

(defn make-pq
  "Priority queue with updatable priorities."
  [] (atom {:heap (sorted-set) :prio {}}))

(defn- pq-minkey [pq]
  (if (empty? (:heap @pq)) Double/POSITIVE_INFINITY (ffirst (:heap @pq))))

(defn- pq-put [pq item priority]
  (swap! pq (fn [{:keys [heap prio] :as s}]
              (let [heap (if (contains? prio item)
                           (disj heap [(get prio item) item]) heap)]
                {:heap (conj heap [priority item])
                 :prio (assoc prio item priority)}))))

(defn- pq-remove [pq item]
  (swap! pq (fn [{:keys [heap prio] :as s}]
              (if (contains? prio item)
                {:heap (disj heap [(get prio item) item])
                 :prio (dissoc prio item)}
                s))))

(defn- pq-top [pq] (second (first (:heap @pq))))

(defn- euclid [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn multi-a-star
  "Returns the start->goal path over an n x n grid with blocked cells,
   or nil when unreachable. heuristics: 0 euclidean, 1 manhattan,
   2 euclidean//t."
  [start goal n blocks & {:keys [w1 w2] :or {w1 1 w2 1}}]
  (let [nh 3
        t (atom 1)
        heuristics {0 (fn [p g] (euclid p g))
                    1 (fn [p g] (+ (Math/abs (- (first p) (first g)))
                                   (Math/abs (- (second p) (second g)))))
                    2 (fn [p g] (long (/ (euclid p g) @t)))}
        g (atom {start 0 goal Double/POSITIVE_INFINITY})
        back (atom {start -1 goal -1})
        open (vec (repeatedly nh make-pq))
        visited (atom #{})
        closed-anchor (atom #{})
        closed-inad (atom #{})
        result (atom nil)
        key-fn (fn [s i] (+ (get @g s) (* w1 ((get heuristics i) s goal))))
        valid? (fn [[x y]] (and (<= 0 x (dec n)) (<= 0 y (dec n))))
        expand (fn [s j]
                 (doseq [q open] (pq-remove q s))
                 (let [[x y] s]
                   (doseq [nb [[(dec x) y] [(inc x) y]
                               [x (inc y)] [x (dec y)]]]
                     (when-not (contains? blocks nb)
                       (when (and (valid? nb) (not (contains? @visited nb)))
                         (swap! visited conj nb)
                         (swap! back assoc nb -1)
                         (swap! g assoc nb Double/POSITIVE_INFINITY))
                       (when (and (valid? nb)
                                  (> (get @g nb) (inc (get @g s))))
                         (swap! g assoc nb (inc (get @g s)))
                         (swap! back assoc nb s)
                         (when-not (contains? @closed-anchor nb)
                           (pq-put (nth open 0) nb (key-fn nb 0))
                           (when-not (contains? @closed-inad nb)
                             (doseq [v (range 1 nh)]
                               (when (<= (key-fn nb v)
                                         (* w2 (key-fn nb 0)))
                                 (pq-put (nth open j) nb
                                         (key-fn nb v)))))))))))
        retrace (fn []
                  (loop [x (get @back goal) path (list goal)]
                    (if (= x start)
                      (vec (cons start path))
                      (recur (get @back x) (cons x path)))))]
    (doseq [i (range nh)] (pq-put (nth open i) start (key-fn start i)))
    (while (and (nil? @result) (< (pq-minkey (nth open 0))
                                  Double/POSITIVE_INFINITY))
      (doseq [i (range 1 nh)]
        (when (nil? @result)
          (cond
            (<= (pq-minkey (nth open i)) (* w2 (pq-minkey (nth open 0))))
            (do (swap! t inc)
                (if (<= (get @g goal) (pq-minkey (nth open i)))
                  (when (< (get @g goal) Double/POSITIVE_INFINITY)
                    (reset! result (retrace)))
                  (let [s (pq-top (nth open i))]
                    (swap! visited conj s)
                    (expand s i)
                    (swap! closed-inad conj s))))
            (<= (get @g goal) (pq-minkey (nth open 0)))
            (when (< (get @g goal) Double/POSITIVE_INFINITY)
              (reset! result (retrace)))
            :else
            (let [s (pq-top (nth open 0))]
              (swap! visited conj s)
              (expand s 0)
              (swap! closed-anchor conj s))))))
    @result))
