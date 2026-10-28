(ns clojure-algorithms.data-structures.linked-list.merge-sort-linked-list
  "Port of data_structures/linked_list/merge_sort_linked_list.py.
  Merge sort over immutable {:data :next} chains.")

(defn make-node
  "Create a chain node with DATA and optional NEXT."
  ([data] (make-node data nil))
  ([data next] {:data data :next next}))

(defn node->str
  "Render like the Python dataclass repr."
  [node]
  (if (nil? node)
    "None"
    (str "Node(data=" (:data node) ", next=" (node->str (:next node)) ")")))

(defn chain-from-vec
  "Chain over the items of V."
  [v]
  (reduce (fn [nxt x] (make-node x nxt)) nil (reverse v)))

(defn iter-linked-list
  "Data sequence of the chain."
  [head]
  (when head
    (cons (:data head) (lazy-seq (iter-linked-list (:next head))))))

(defn get-middle
  "Middle node (slow/fast pointers); nil for empty/single chains."
  [head]
  (when (and head (:next head))
    (loop [slow head fast (:next head)]
      (if (and fast (:next fast))
        (recur (:next slow) (:next (:next fast)))
        slow))))

(defn merge-chains
  "Merge two sorted chains."
  [left right]
  (cond
    (nil? left) right
    (nil? right) left
    (<= (:data left) (:data right))
    (make-node (:data left) (merge-chains (:next left) right))
    :else (make-node (:data right) (merge-chains left (:next right)))))

(defn merge-sort-linked-list
  "Sorted chain."
  [head]
  (if (or (nil? head) (nil? (:next head)))
    head
    (let [items (vec (iter-linked-list head))
          mid (loop [slow 0 fast 1]
                (if (and (< fast (count items)) (< (inc fast) (count items)))
                  (recur (inc slow) (+ fast 2))
                  slow))
          left (merge-sort-linked-list
                (chain-from-vec (subvec items 0 (inc mid))))
          right (merge-sort-linked-list
                 (chain-from-vec (subvec items (inc mid))))]
      (merge-chains left right))))
