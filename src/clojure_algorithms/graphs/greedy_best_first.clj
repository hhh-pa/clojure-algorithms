(ns clojure-algorithms.graphs.greedy-best-first
  "Greedy best-first grid search with Manhattan heuristic.")

(def delta "Moves: up, left, down, right." [[-1 0] [0 -1] [1 0] [0 1]])

(defn- make-node [x y gx gy g parent]
  {:pos [y x] :x x :y y :gx gx :gy gy :g g :parent parent
   :f (+ (Math/abs (- x gx)) (Math/abs (- y gy)))})

(defn- successors [grid target parent]
  (into []
        (comp (map (fn [[dy dx]] [(+ (:x parent) dx) (+ (:y parent) dy)]))
              (filter (fn [[x y]]
                        (and (<= 0 x (dec (count (first grid))))
                             (<= 0 y (dec (count grid)))
                             (zero? (get-in grid [y x])))))
              (map (fn [[x y]]
                     (make-node x y (:gx target) (:gy target)
                                (inc (:g parent)) parent))))
        delta))

(defn- retrace [node]
  (loop [n node path (list (:pos node))]
    (if-let [p (:parent n)]
      (recur p (cons (:pos p) path))
      (vec path))))

(defn- first-min-by
  "Element with minimal (k %), keeping the first on ties."
  [k coll]
  (reduce (fn [a b] (if (< (k b) (k a)) b a)) coll))

(defn search
  "Returns the start->goal path, or [start] when unreachable."
  [grid start goal]
  (let [start-node (make-node (second start) (first start)
                              (second goal) (first goal) 0 nil)
        target (make-node (second goal) (first goal)
                          (second goal) (first goal) 99999 nil)]
    (loop [open [start-node] closed []]
      (if (empty? open)
        [(:pos start-node)]
        (let [current (first-min-by :f open)
              open (vec (remove #(= % current) open))]
          (if (= (:pos current) (:pos target))
            (retrace current)
            (let [closed (conj closed current)
                  kids (remove (fn [k]
                                 (or (some #(= (:pos k) (:pos %)) closed)
                                     (some #(= (:pos k) (:pos %)) open)))
                               (successors grid target current))]
              (recur (into open kids) closed))))))))
