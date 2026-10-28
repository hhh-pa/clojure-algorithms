(ns clojure-algorithms.data-structures.binary-tree.splay-tree
  "Port of data_structures/binary_tree/splay_tree.py.
  Functional splay tree. Nodes are {:key :left :right} maps;
  a tree is its root (nil when empty).")

(defn rotate-right-node
  "Right rotation around NODE."
  [node]
  (let [left (:left node)]
    (assoc left :right (assoc node :left (:right left)))))

(defn rotate-left-node
  "Left rotation around NODE."
  [node]
  (let [right (:right node)]
    (assoc right :left (assoc node :right (:left right)))))

(defn splay
  "Splay KEY (or the last node on the search path) to the root."
  [root key]
  (cond
    (or (nil? root) (= (:key root) key)) root
    (< key (:key root))
    (if (nil? (:left root))
      root
      (let [left (:left root)]
        (cond
          (< key (:key left))
          (let [root (assoc root :left (assoc left :left
                                              (splay (:left left) key)))
                root (rotate-right-node root)]
            (if (nil? (:left root)) root (rotate-right-node root)))
          (> key (:key left))
          (let [new-right (splay (:right left) key)
                left (assoc left :right new-right)
                left (if (nil? new-right) left (rotate-left-node left))
                root (assoc root :left left)]
            (if (nil? (:left root)) root (rotate-right-node root)))
          :else (rotate-right-node root))))
    :else
    (if (nil? (:right root))
      root
      (let [right (:right root)]
        (cond
          (> key (:key right))
          (let [root (assoc root :right
                            (assoc right :right
                                   (splay (:right right) key)))
                root (rotate-left-node root)]
            (if (nil? (:right root)) root (rotate-left-node root)))
          (< key (:key right))
          (let [new-left (splay (:left right) key)
                right (assoc right :left new-left)
                right (if (nil? new-left) right (rotate-right-node right))
                root (assoc root :right right)]
            (if (nil? (:right root)) root (rotate-left-node root)))
          :else (rotate-left-node root))))))

(defn splay-insert
  "Insert KEY (duplicates ignored); returns the new root."
  [root key]
  (if (nil? root)
    {:key key :left nil :right nil}
    (let [root (splay root key)]
      (if (= (:key root) key)
        root
        (if (< key (:key root))
          {:key key :left (:left root)
           :right (assoc root :left nil)}
          {:key key :left (assoc root :right nil)
           :right (:right root)})))))

(defn splay-search
  "Vector [found? new-root] for KEY."
  [root key]
  (let [root (splay root key)]
    [(and (some? root) (= (:key root) key)) root]))

(defn splay-delete
  "Delete KEY when present; returns the new root."
  [root key]
  (if (nil? root)
    nil
    (let [root (splay root key)]
      (if (not= (:key root) key)
        root
        (let [left (:left root) right (:right root)]
          (if (nil? left)
            right
            (let [left (splay left key)]
              (assoc left :right right))))))))

(defn inorder-keys
  "Inorder key sequence."
  [root]
  (when root
    (concat (inorder-keys (:left root)) [(:key root)]
            (inorder-keys (:right root)))))
