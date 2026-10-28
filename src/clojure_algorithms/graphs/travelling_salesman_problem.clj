(ns clojure-algorithms.graphs.travelling-salesman-problem
  "Travelling salesman via brute force and Held-Karp dynamic programming.")

(defn euclidean-distance
  "Euclidean distance between 2D points."
  [[x1 y1] [x2 y2]]
  (if-not (and (number? x1) (number? y1) (number? x2) (number? y2))
    (throw (IllegalArgumentException.
            "Invalid input: Points must be numerical coordinates"))
    (Math/sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))))

(defn validate-graph
  "Throws ex-info when the node->coordinate map is invalid."
  [graph-points]
  (when-not (map? graph-points)
    (throw (ex-info "Graph must be a dictionary with node names and coordinates"
                    {:type :invalid-graph})))
  (doseq [[node coords] graph-points]
    (when-not (and (string? node) (vector? coords) (= 2 (count coords))
                   (every? number? coords))
      (throw (ex-info "Each node must have a valid 2D coordinate [x, y]"
                      {:type :invalid-graph})))))

(defn- permutations [coll]
  (if (empty? coll)
    [[]]
    (vec (mapcat (fn [i]
                   (map #(cons (nth coll i) %)
                        (permutations (vec (concat (subvec (vec coll) 0 i)
                                                   (subvec (vec coll) (inc i)))))))
                 (range (count coll))))))

(defn travelling-salesman-brute-force
  "Returns [best-path best-distance] by trying all permutations."
  [graph-points]
  (validate-graph graph-points)
  (let [nodes (vec (keys graph-points))]
    (when (< (count nodes) 2)
      (throw (ex-info "Graph must have at least two nodes"
                      {:type :invalid-graph})))
    (let [start (first nodes)
          perms (permutations (subvec nodes 1))]
      (reduce (fn [[bp bd] perm]
                (let [path (vec (concat [start] perm [start]))
                      total (reduce + (map #(euclidean-distance
                                             (get graph-points %1)
                                             (get graph-points %2))
                                           path (rest path)))]
                  (if (< total bd) [path total] [bp bd])))
              [[] Double/POSITIVE_INFINITY] perms))))

(defn travelling-salesman-dynamic-programming
  "Returns [best-path best-distance] via Held-Karp."
  [graph-points]
  (validate-graph graph-points)
  (let [nodes (vec (keys graph-points))
        n (count nodes)]
    (when (< n 2)
      (throw (ex-info "Graph must have at least two nodes"
                      {:type :invalid-graph})))
    (let [dist (vec (for [i (range n)]
                      (vec (for [j (range n)]
                             (euclidean-distance (get graph-points (nth nodes i))
                                                 (get graph-points (nth nodes j)))))))
          dp (atom (vec (repeat (bit-shift-left 1 n)
                                (vec (repeat n Double/POSITIVE_INFINITY)))))]
      (swap! dp assoc-in [1 0] 0.0)
      (doseq [mask (range (bit-shift-left 1 n))
              u (range n)
              :when (pos? (bit-and mask (bit-shift-left 1 u)))
              v (range n)
              :when (zero? (bit-and mask (bit-shift-left 1 v)))]
        (let [nm (bit-or mask (bit-shift-left 1 v))]
          (swap! dp assoc-in [nm v]
                 (min (get-in @dp [nm v])
                      (+ (get-in @dp [mask u]) (get-in dist [u v]))))))
      (let [final (dec (bit-shift-left 1 n))
            [min-cost end-node]
            (reduce (fn [[mc en] u]
                      (let [c (+ (get-in @dp [final u]) (get-in dist [u 0]))]
                        (if (> mc c) [c u] [mc en])))
                    [Double/POSITIVE_INFINITY -1] (range 1 n))]
        (loop [mask final node end-node path []]
          (if (zero? node)
            [(vec (concat [(first nodes)] (reverse path) [(first nodes)]))
             min-cost]
            (let [path (conj path (nth nodes node))
                  u (first (filter #(and (pos? (bit-and mask (bit-shift-left 1 %)))
                                         (= (get-in @dp [mask node])
                                            (+ (get-in @dp [(bit-xor mask (bit-shift-left 1 node)) %])
                                               (get-in dist [% node]))))
                                   (range n)))]
              (recur (bit-xor mask (bit-shift-left 1 node)) u path))))))))
