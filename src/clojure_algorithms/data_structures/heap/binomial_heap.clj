(ns clojure-algorithms.data-structures.heap.binomial-heap
  "Port of data_structures/heap/binomial_heap.py.
  Min-oriented binomial heap. This port uses the classic functional
  representation (rank-annotated binomial trees) rather than the
  parent-pointer structure of the Python version; the observable
  insert/merge/peek/delete-min behavior is the same.
  Trees are {:rank :root :children}; a heap is {:trees [...] :size n}.")

(defn- link-trees [t1 t2]
  (assert (= (:rank t1) (:rank t2)))
  (if (<= (:root t1) (:root t2))
    {:rank (inc (:rank t1)) :root (:root t1)
     :children (cons t2 (:children t1))}
    {:rank (inc (:rank t1)) :root (:root t2)
     :children (cons t1 (:children t2))}))

(defn- insert-tree [trees t]
  (if (empty? trees)
    (list t)
    (let [u (first trees)]
      (if (< (:rank t) (:rank u))
        (cons t trees)
        (insert-tree (rest trees) (link-trees t u))))))

(defn- merge-trees [ts1 ts2]
  (cond
    (empty? ts1) ts2
    (empty? ts2) ts1
    :else (let [t1 (first ts1) t2 (first ts2)]
            (cond
              (< (:rank t1) (:rank t2))
              (cons t1 (merge-trees (rest ts1) ts2))
              (< (:rank t2) (:rank t1))
              (cons t2 (merge-trees ts1 (rest ts2)))
              :else (insert-tree (merge-trees (rest ts1) (rest ts2))
                                 (link-trees t1 t2))))))

(defn make-binomial-heap
  "Empty binomial heap."
  []
  {:trees () :size 0})

(defn heap-size
  "Number of elements."
  [h]
  (:size h))

(defn heap-empty?
  "True when the heap is empty."
  [h]
  (zero? (:size h)))

(defn heap-insert
  "Insert VAL; returns the new heap."
  [{:keys [trees size]} val]
  {:trees (insert-tree trees {:rank 0 :root val :children ()})
   :size (inc size)})

(defn heap-merge
  "Meld H1 and H2; returns the new heap."
  [h1 h2]
  {:trees (merge-trees (:trees h1) (:trees h2))
   :size (+ (:size h1) (:size h2))})

(defn- min-tree [trees]
  (reduce (fn [a b] (if (<= (:root a) (:root b)) a b)) trees))

(defn heap-peek
  "Minimum value."
  [{:keys [trees]}]
  (:root (min-tree trees)))

(defn heap-delete-min
  "Vector [min-value new-heap]."
  [{:keys [trees size]}]
  (let [t (min-tree trees)
        rest-trees (remove #(identical? % t) trees)]
    [(:root t)
     {:trees (merge-trees (reverse (:children t)) rest-trees)
      :size (dec size)}]))

(defn heap-drain
  "Sorted vector of all values."
  [h]
  (loop [h h out []]
    (if (heap-empty? h)
      out
      (let [[m h] (heap-delete-min h)]
        (recur h (conj out m))))))
