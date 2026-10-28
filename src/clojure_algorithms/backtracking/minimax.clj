(ns clojure-algorithms.backtracking.minimax
  "Minimax for a two-player game on a binary tree: the maximizer picks the
  largest achievable leaf score, the minimizer the smallest.
  `depth` is the current depth, `node-index` the node position, `scores`
  the leaf scores (a power of two in count), and `height` the tree height.")

(defn- power-of-two?
  [n]
  (and (pos? n) (zero? (bit-and n (dec n)))))

(defn minimax
  "Returns the optimal score from this node: max of the children when
  `max?` is true, min otherwise."
  [depth node-index max? scores height]
  (when (neg? depth)
    (throw (ex-info "Depth cannot be less than 0" {})))
  (when (empty? scores)
    (throw (ex-info "Scores cannot be empty" {})))
  (when (not (power-of-two? (count scores)))
    (throw (ex-info "Number of scores must be a power of 2" {})))
  ;; >= (not =): float heights like log2(8) may not equal an int exactly.
  (if (>= depth height)
    (nth scores node-index)
    (let [left (minimax (inc depth) (* 2 node-index) (not max?) scores height)
          right (minimax (inc depth) (inc (* 2 node-index)) (not max?) scores height)]
      (if max? (max left right) (min left right)))))

(defn optimal-value
  "Returns the optimal value of the game described by leaf `scores`."
  [scores]
  (minimax 0 0 true scores
           (Math/round (/ (Math/log (count scores)) (Math/log 2)))))
