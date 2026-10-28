(ns clojure-algorithms.data-structures.binary-tree.treap
  "Port of data_structures/binary_tree/treap.py.
  Randomized treap (BST by value, heap by priority).
  Nodes are {:value :prior :left :right} maps.")

(defn make-node
  "Create a treap node with VALUE and a random priority."
  [value]
  {:value value :prior (rand) :left nil :right nil})

(defn treap-split
  "Split ROOT into [< value, >= value] trees."
  [root value]
  (cond
    (or (nil? root) (nil? (:value root))) [nil nil]
    (<= value (:value root))
    (let [[l r] (treap-split (:left root) value)]
      [l (assoc root :left r)])
    :else
    (let [[l r] (treap-split (:right root) value)]
      [(assoc root :right l) r])))

(defn treap-merge
  "Merge LEFT (< all of RIGHT) and RIGHT."
  [left right]
  (cond
    (nil? left) right
    (nil? right) left
    (> (:prior left) (:prior right))
    (assoc left :right (treap-merge (:right left) right))
    :else (assoc right :left (treap-merge left (:left right)))))

(defn treap-insert
  "Insert VALUE; returns the new root."
  [root value]
  (let [node (make-node value)
        [l r] (treap-split root value)]
    (treap-merge (treap-merge l node) r)))

(defn treap-erase
  "Erase all nodes holding VALUE; returns the new root."
  [root value]
  (let [[l r] (treap-split root value)
        [_ r] (treap-split r (inc value))]
    (treap-merge l r)))

(defn inorder-keys
  "Inorder value sequence."
  [root]
  (when root
    (concat (inorder-keys (:left root)) [(:value root)]
            (inorder-keys (:right root)))))

(defn interact-treap
  "Apply space-separated \"+v\"/\"-v\" commands to ROOT; returns the root.
  Unknown commands print \"Unknown command\" like the Python version."
  [root args]
  (reduce (fn [root arg]
            (case (first arg)
              \+ (treap-insert root (Integer/parseInt (subs arg 1)))
              \- (treap-erase root (Integer/parseInt (subs arg 1)))
              (do (println "Unknown command") root)))
          root
          (clojure.string/split args #"\s+")))
