(ns clojure-algorithms.data-structures.heap.skew-heap
  "Port of data_structures/heap/skew_heap.py.
  Skew min-heap. Nodes are {:value :left :right} maps;
  a heap is its root (nil when empty).")

(defn merge-nodes
  "Merge two heap roots."
  [root1 root2]
  (cond
    (nil? root1) root2
    (nil? root2) root1
    :else (let [[root1 root2] (if (> (:value root1) (:value root2))
                                [root2 root1] [root1 root2])]
            (assoc root1 :right (:left root1)
                   :left (merge-nodes (:right root1) root2)))))

(defn heap-insert
  "Insert VALUE; returns the new root."
  [root value]
  (merge-nodes root {:value value :left nil :right nil}))

(defn heap-top
  "Minimum value. Raises on an empty heap."
  [root]
  (if (nil? root)
    (throw (ex-info "Can't get top element for the empty heap."
                    {:type :index-error}))
    (:value root)))

(defn heap-pop
  "Vector [min-value new-root]. Raises on an empty heap."
  [root]
  (let [result (heap-top root)]
    [result (merge-nodes (:left root) (:right root))]))

(defn heap-clear
  "The empty heap."
  [_]
  nil)

(defn to-sorted-list
  "Sorted vector of the heap contents."
  [root]
  (loop [root root out []]
    (if (nil? root)
      out
      (let [[m root] (heap-pop root)]
        (recur root (conj out m))))))

(defn make-skew-heap
  "Heap built from DATA."
  [data]
  (reduce heap-insert nil data))
