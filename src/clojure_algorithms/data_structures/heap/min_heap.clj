(ns clojure-algorithms.data-structures.heap.min-heap
  "Port of data_structures/heap/min_heap.py.
  Min-heap of named nodes with decrease-key.
  State is {:heap [...] :idx-of {name idx} :heap-dict {name val}}.
  Nodes are {:name :val} maps keyed by name.")

(defn make-node
  "Create a heap node with NAME and VAL."
  [name val]
  {:name name :val val})

(defn node->str
  "Render NODE like the Python str."
  [node]
  (str "Node(" (:name node) ", " (:val node) ")"))

(defn parent-idx [idx] (quot (dec idx) 2))
(defn left-idx [idx] (inc (* 2 idx)))
(defn right-idx [idx] (+ (* 2 idx) 2))

(defn- sift-down [h idx]
  (loop [h h idx idx]
    (let [heap (:heap h)
          n (count heap)
          left (left-idx idx)
          right (right-idx idx)
          s1 (if (and (< left n)
                      (< (:val (nth heap left)) (:val (nth heap idx))))
               left idx)
          s2 (if (and (< right n)
                      (< (:val (nth heap right)) (:val (nth heap s1))))
               right s1)]
      (if (= s2 idx)
        h
        (let [a (nth heap idx) b (nth heap s2)]
          (recur (-> h
                     (assoc :heap (assoc heap idx b s2 a))
                     (assoc :idx-of (assoc (:idx-of h) (:name a) s2
                                           (:name b) idx)))
                 s2))))))

(defn- sift-node-up [{:keys [heap idx-of] :as h} idx]
  (loop [h h idx idx]
    (let [p (parent-idx idx)]
      (if (and (>= p 0)
               (> (:val (nth (:heap h) p)) (:val (nth (:heap h) idx))))
        (let [heap (:heap h)
              a (nth heap p) b (nth heap idx)]
          (recur (-> h
                     (assoc :heap (assoc heap p b idx a))
                     (assoc :idx-of (assoc (:idx-of h) (:name a) idx
                                           (:name b) p)))
                 p))
        h))))

(defn make-min-heap
  "Min-heap over the node sequence ARRAY."
  [array]
  (let [v (vec array)
        h {:heap v
           :idx-of (into {} (map-indexed (fn [i n] [(:name n) i])) v)
           :heap-dict (into {} (map (fn [n] [(:name n) (:val n)])) v)}
        last-idx (dec (count v))]
    (reduce sift-down h (range (parent-idx last-idx) -1 -1))))

(defn heap-peek
  "Minimum node."
  [{:keys [heap]}]
  (first heap))

(defn heap-remove
  "Vector [min-node new-heap]."
  [{:keys [heap idx-of] :as h}]
  (let [last-idx (dec (count heap))
        a (nth heap 0) b (nth heap last-idx)
        h (-> h
              (assoc :heap (assoc heap 0 b last-idx a))
              (assoc :idx-of (assoc idx-of (:name a) last-idx
                                    (:name b) 0)))]
    (let [x (peek (:heap h))
          h (-> h
                (update :heap pop)
                (update :idx-of dissoc (:name x)))]
      [x (sift-down h 0)])))

(defn heap-insert
  "Insert NODE; returns the new heap."
  [{:keys [heap] :as h} node]
  (let [h (-> h
              (update :heap conj node)
              (assoc-in [:idx-of (:name node)] (count heap))
              (assoc-in [:heap-dict (:name node)] (:val node)))]
    (sift-node-up h (dec (count (:heap h))))))

(defn heap-empty?
  "True when the heap is empty."
  [{:keys [heap]}]
  (empty? heap))

(defn decrease-key
  "Lower NODE's value to NEW-VALUE; returns the new heap."
  [{:keys [heap idx-of] :as h} node new-value]
  (let [idx (get idx-of (:name node))]
    (when-not (> (:val (nth heap idx)) new-value)
      (throw (ex-info "newValue must be less that current value"
                      {:type :assertion-error})))
    (let [node (assoc node :val new-value)]
      (-> h
          (assoc-in [:heap idx] node)
          (assoc-in [:heap-dict (:name node)] new-value)
          (sift-node-up idx)))))

(defn heap-value
  "Current value recorded for key NAME."
  [{:keys [heap-dict]} key]
  (get heap-dict key))
