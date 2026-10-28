(ns clojure-algorithms.data-structures.kd-tree.nearest-neighbour-search
  "Port of data_structures/kd_tree/nearest_neighbour_search.py.
  Returns [nearest-point squared-distance nodes-visited].")

(defn- squared-dist [a b]
  (reduce + (map (fn [x y] (let [d (- x y)] (* d d))) a b)))

(defn nearest-neighbour-search
  "Nearest point in the KD-tree ROOT to QUERY-POINT."
  [root query-point]
  (let [state (atom {:point nil :dist ##Inf :visited 0})]
    (letfn [(search [node depth]
              (when node
                (swap! state update :visited inc)
                (let [d (squared-dist query-point (:point node))]
                  (when (or (nil? (:point @state)) (< d (:dist @state)))
                    (swap! state assoc :point (:point node) :dist d)))
                (let [k (count query-point)
                      axis (mod depth k)
                      [nearer further]
                      (if (<= (nth query-point axis)
                              (nth (:point node) axis))
                        [(:left node) (:right node)]
                        [(:right node) (:left node)])]
                  (search nearer (inc depth))
                  (let [diff (- (nth query-point axis)
                                (nth (:point node) axis))]
                    (when (< (* diff diff) (:dist @state))
                      (search further (inc depth)))))))]
      (search root 0)
      [(:point @state) (:dist @state) (:visited @state)])))
