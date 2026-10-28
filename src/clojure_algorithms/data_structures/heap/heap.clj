(ns clojure-algorithms.data-structures.heap.heap
  "Port of data_structures/heap/heap.py.
  CLRS max-heap over immutable vectors. State is {:h [...] :heap-size n}.")

(defn make-heap
  "Empty max-heap."
  []
  {:h [] :heap-size 0})

(defn heap-values
  "Heap contents."
  [{:keys [h]}]
  h)

(defn parent-index
  "Parent index of CHILD-IDX, or nil."
  [_ child-idx]
  (when-not (number? child-idx)
    (throw (ex-info (str "'>' not supported between instances of '"
                         (type child-idx) "' and 'int'")
                    {:type :type-error})))
  (when (pos? child-idx)
    (quot (dec child-idx) 2)))

(defn left-child-idx
  "Left child index of PARENT-IDX, or nil."
  [{:keys [heap-size]} parent-idx]
  (let [i (inc (* 2 parent-idx))]
    (when (< i heap-size) i)))

(defn right-child-idx
  "Right child index of PARENT-IDX, or nil."
  [{:keys [heap-size]} parent-idx]
  (let [i (+ (* 2 parent-idx) 2)]
    (when (< i heap-size) i)))

(defn max-heapify
  "Restore the heap property at INDEX; returns the new heap."
  [{:keys [h] :as heap} index]
  (if (>= index (:heap-size heap))
    heap
    (let [left (left-child-idx heap index)
          right (right-child-idx heap index)
          v1 (if (and left (> (nth h left) (nth h index))) left index)
          v2 (if (and right (> (nth h right) (nth h v1))) right v1)]
      (if (= v2 index)
        heap
        (recur (assoc heap :h (assoc h v2 (nth h index) index (nth h v2)))
               v2)))))

(defn build-max-heap
  "Heap built from COLLECTION."
  [coll]
  (let [h {:h (vec coll) :heap-size (count coll)}]
    (if (> (:heap-size h) 1)
      (reduce max-heapify h
              (range (dec (quot (:heap-size h) 2)) -1 -1))
      h)))

(defn extract-max
  "Vector [max-value new-heap]."
  [{:keys [h heap-size] :as heap}]
  (cond
    (>= heap-size 2)
    [(nth h 0)
     (max-heapify (-> heap
                      (assoc-in [:h 0] (peek h))
                      (update :h pop)
                      (update :heap-size dec))
                  0)]
    (= heap-size 1)
    [(peek h) (-> heap (update :h pop) (update :heap-size dec))]
    :else (throw (ex-info "Empty heap" {:type :exception}))))

(defn heap-insert
  "Insert VALUE; returns the new heap."
  [{:keys [h heap-size] :as heap} value]
  (let [floordiv2 (fn [x] (int (Math/floor (/ x 2))))]
    (loop [heap (-> heap (update :h conj value) (update :heap-size inc))
           idx (floordiv2 (dec heap-size))]
      (if (neg? idx)
        heap
        (recur (max-heapify heap idx) (floordiv2 (dec idx)))))))

(defn heap-sort
  "New heap with contents sorted ascending."
  [{:keys [heap-size] :as heap}]
  (let [heap (loop [heap heap j (dec heap-size)]
               (if (<= j 0)
                 heap
                 (recur (max-heapify
                         (-> heap
                             (assoc :h (assoc (:h heap) 0 (nth (:h heap) j)
                                              j (nth (:h heap) 0)))
                             (update :heap-size dec))
                         0)
                        (dec j))))]
    (assoc heap :heap-size heap-size)))
