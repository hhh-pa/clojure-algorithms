(ns clojure-algorithms.data-structures.binary-tree.red-black-tree
  "Port of data_structures/binary_tree/red_black_tree.py.
  Functional red-black tree (Okasaki insertion balancing).
  Nodes are {:label :color :left :right} maps with :red/:black colors;
  nil is the empty (black) leaf. Deletion rebuilds from the remaining
  keys, preserving all red-black invariants.
  The public querying API (search/floor/ceil/min/max/traversals and the
  color/black-height checkers) mirrors the Python version.")

(defn make-node
  "Create a node with LABEL, COLOR and optional children."
  ([label] (make-node label :black nil nil))
  ([label color left right]
   {:label label :color color :left left :right right}))

(defn- node-color [node]
  (if (nil? node) :black (:color node)))

(defn- balance [color left label right]
  (cond
    (and (= color :black) (= (node-color left) :red)
         (= (node-color (:left left)) :red))
    (make-node (:label left) :red
               (assoc (:left left) :color :black)
               (make-node label :black (:right left) right))
    (and (= color :black) (= (node-color left) :red)
         (= (node-color (:right left)) :red))
    (make-node (:label (:right left)) :red
               (make-node (:label left) :black (:left left)
                          (:left (:right left)))
               (make-node label :black (:right (:right left)) right))
    (and (= color :black) (= (node-color right) :red)
         (= (node-color (:left right)) :red))
    (make-node (:label (:left right)) :red
               (make-node label :black left (:left (:left right)))
               (make-node (:label right) :black (:right (:left right))
                          (:right right)))
    (and (= color :black) (= (node-color right) :red)
         (= (node-color (:right right)) :red))
    (make-node (:label right) :red
               (make-node label :black left (:left right))
               (assoc (:right right) :color :black))
    :else (make-node label color left right)))

(defn- ins [node label]
  (cond
    (nil? node) (make-node label :red nil nil)
    (< label (:label node))
    (balance (:color node) (ins (:left node) label) (:label node)
             (:right node))
    (> label (:label node))
    (balance (:color node) (:left node) (:label node)
             (ins (:right node) label))
    :else node))

(defn rb-insert
  "Insert LABEL; returns the new root."
  [root label]
  (let [t (ins root label)]
    (assoc t :color :black)))

(defn inorder-labels
  "Inorder label sequence."
  [root]
  (when root
    (concat (inorder-labels (:left root)) [(:label root)]
            (inorder-labels (:right root)))))

(defn preorder-labels
  "Preorder label sequence."
  [root]
  (when root
    (concat [(:label root)]
            (preorder-labels (:left root))
            (preorder-labels (:right root)))))

(defn postorder-labels
  "Postorder label sequence."
  [root]
  (when root
    (concat (postorder-labels (:left root))
            (postorder-labels (:right root))
            [(:label root)])))

(defn rb-remove
  "Remove LABEL; returns the new root."
  [root label]
  (reduce rb-insert nil (remove #(= % label) (inorder-labels root))))

(defn rb-contains?
  "True when LABEL is in the tree."
  [root label]
  (loop [node root]
    (cond
      (nil? node) false
      (= label (:label node)) true
      (< label (:label node)) (recur (:left node))
      :else (recur (:right node)))))

(defn rb-search
  "Node holding LABEL, or nil."
  [root label]
  (loop [node root]
    (cond
      (nil? node) nil
      (= label (:label node)) node
      (< label (:label node)) (recur (:left node))
      :else (recur (:right node)))))

(defn rb-floor
  "Greatest label at most LABEL, or nil."
  [root label]
  (loop [node root best nil]
    (cond
      (nil? node) best
      (= (:label node) label) label
      (> (:label node) label) (recur (:left node) best)
      :else (recur (:right node) (:label node)))))

(defn rb-ceil
  "Smallest label at least LABEL, or nil."
  [root label]
  (loop [node root best nil]
    (cond
      (nil? node) best
      (= (:label node) label) label
      (< (:label node) label) (recur (:right node) best)
      :else (recur (:left node) (:label node)))))

(defn rb-max
  "Maximum label, or nil."
  [root]
  (when root
    (loop [node root]
      (if (:right node) (recur (:right node)) (:label node)))))

(defn rb-min
  "Minimum label, or nil."
  [root]
  (when root
    (loop [node root]
      (if (:left node) (recur (:left node)) (:label node)))))

(defn rb-count
  "Number of nodes."
  [root]
  (count (inorder-labels root)))

(defn rotate-left-node
  "Left rotation around NODE (pure, local)."
  [node]
  (if (nil? (:right node))
    node
    (let [right (:right node)]
      (assoc right :left (assoc node :right (:left right))))))

(defn rotate-right-node
  "Right rotation around NODE (pure, local)."
  [node]
  (if (nil? (:left node))
    node
    (let [left (:left node)]
      (assoc left :right (assoc node :left (:right left))))))

(defn- no-red-red? [node]
  (or (nil? node)
      (and (or (not= (:color node) :red)
               (and (not= (node-color (:left node)) :red)
                    (not= (node-color (:right node)) :red)))
           (no-red-red? (:left node))
           (no-red-red? (:right node)))))

(defn black-height
  "Black height of the tree, or nil when inconsistent."
  [root]
  (letfn [(bh [node]
            (if (nil? node)
              0
              (let [l (bh (:left node))
                    r (bh (:right node))]
                (when (and l r (= l r))
                  (+ l (if (= (:color node) :black) 1 0))))))]
    (bh root)))

(defn check-color-properties?
  "True when red-black color invariants hold."
  [root]
  (boolean (and (no-red-red? root) (black-height root))))
