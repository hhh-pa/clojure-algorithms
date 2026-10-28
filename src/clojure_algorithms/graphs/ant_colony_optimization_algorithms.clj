(ns clojure-algorithms.graphs.ant-colony-optimization-algorithms
  "Ant colony optimization for the travelling salesman problem.")

(defn distance
  "Euclidean distance between two [x y] cities."
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn pheromone-update
  "Evaporates and deposits pheromones; returns [pheromone best-path best-dist]."
  [pheromone cities evaporation ants-routes q best-path best-distance]
  (let [n (count cities)
        ph (atom (mapv #(mapv * % (repeat evaporation)) pheromone))
        best (atom [best-path best-distance])]
    (doseq [route ants-routes]
      (let [total (reduce + (map #(distance (get cities %1) (get cities %2))
                                 route (rest route)))
            delta (/ q total)]
        (doseq [[a b] (map vector route (rest route))]
          (swap! ph assoc-in [a b] (+ (get-in @ph [a b]) delta))
          (swap! ph assoc-in [b a] (get-in @ph [a b])))
        (when (< total (second @best))
          (reset! best [route total]))))
    [(mapv vec @ph) (first @best) (second @best)]))

(defn- weighted-choice [candidates weights]
  (let [total (reduce + weights)
        r (* (Math/random) total)]
    (loop [cs candidates ws weights acc 0]
      (let [acc (+ acc (first ws))]
        (if (or (> acc r) (empty? (rest cs)))
          (first cs)
          (recur (rest cs) (rest ws) acc))))))

(defn city-select
  "Chooses the next city for one ant; returns [chosen remaining]."
  [pheromone current-city unvisited-cities alpha beta]
  (let [[cur-id cur-pos] (first current-city)
        cands (vec unvisited-cities)
        probs (mapv (fn [[id pos]]
                      (* (Math/pow (get-in pheromone [id cur-id]) alpha)
                         (Math/pow (/ 1 (distance pos cur-pos)) beta)))
                    cands)
        chosen (weighted-choice (mapv first cands) probs)]
    [{chosen (get unvisited-cities chosen)} (dissoc unvisited-cities chosen)]))

(defn ant-colony-tsp
  "Runs the ACO main loop; returns [best-path best-distance]."
  [cities ants-num iterations-num evaporation alpha beta q]
  (let [n (count cities)
        pheromone (atom (vec (repeat n (vec (repeat n 1.0)))))
        best (atom [[] Double/POSITIVE_INFINITY])]
    (dotimes [_ iterations-num]
      (let [routes
            (doall
             (for [_ (range ants-num)]
               (let [start-id (first (keys cities))]
                 (loop [current {start-id (get cities start-id)}
                        unvisited (dissoc cities start-id)
                        route [start-id]]
                   (if (empty? unvisited)
                     (conj route 0)
                     (let [[chosen rest-cities]
                           (city-select @pheromone current unvisited
                                        alpha beta)]
                       (recur chosen rest-cities
                              (conj route (first (keys chosen))))))))))]
        (let [[ph bp bd] (pheromone-update @pheromone cities evaporation
                                           routes q (first @best)
                                           (second @best))]
          (reset! pheromone ph)
          (reset! best [bp bd]))))
    @best))
