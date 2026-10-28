(ns clojure-algorithms.data-structures.linked-list.sorted-linked-list
  "Port of data_structures/linked_list/sorted_linked_list.py.
  Sorted list backed by a sorted vector.")

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

(defn make-list
  "Empty sorted list."
  []
  [])

(defn list-empty?
  "True when the list is empty."
  [lst]
  (empty? lst))

(defn sorted-insert
  "Insert DATA keeping order (duplicates go last); new list."
  [lst data]
  (let [i (count (take-while #(<= % data) lst))]
    (vec (concat (subvec lst 0 i) [data] (subvec lst i)))))

(defn sorted-delete
  "Delete the first DATA; returns [deleted? new-list]."
  [lst data]
  (let [i (.indexOf ^java.util.List lst data)]
    (if (neg? i)
      [false lst]
      [true (vec (concat (subvec lst 0 i) (subvec lst (inc i))))])))

(defn list-contains?
  "True when DATA is present."
  [lst data]
  (boolean (some #(= % data) lst)))

(defn min-value
  "Minimum value, or nil."
  [lst]
  (first lst))

(defn max-value
  "Maximum value, or nil."
  [lst]
  (peek (vec lst)))

(defn remove-duplicates
  "Deduplicated list."
  [lst]
  (vec (dedupe lst)))

(defn merge-other
  "Insert all of OTHER's items; returns the new list."
  [lst other]
  (reduce sorted-insert (vec lst) other))

(defn ll-str
  "Elements joined with \", \"."
  [lst]
  (clojure.string/join ", " (map str lst)))
