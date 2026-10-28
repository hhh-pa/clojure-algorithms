(ns clojure-algorithms.data-structures.binary-tree.distribute-coins
  "Port of data_structures/binary_tree/distribute_coins.py.
  Minimum moves to give every node exactly one coin.")

(defn make-node
  "Create a node with DATA coins and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn- count-nodes [node]
  (if (nil? node) 0
      (+ (count-nodes (:left node)) (count-nodes (:right node)) 1)))

(defn- count-coins [node]
  (if (nil? node) 0
      (+ (count-coins (:left node)) (count-coins (:right node)) (:data node))))

(defn- get-distrib [node]
  (if (nil? node)
    [0 1]
    (let [[lm le] (get-distrib (:left node))
          [rm re] (get-distrib (:right node))
          to-left (- 1 le)
          to-right (- 1 re)]
      [(+ lm rm (Math/abs to-left) (Math/abs to-right))
       (- (:data node) to-left to-right)])))

(defn distribute-coins
  "Minimum moves so every node holds exactly one coin."
  [root]
  (if (nil? root)
    0
    (do
      (when-not (= (count-nodes root) (count-coins root))
        (throw (ex-info "The nodes number should be same as the number of coins"
                        {:type :value-error})))
      (first (get-distrib root)))))
