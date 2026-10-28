(ns clojure-algorithms.data-structures.heap.heap-generic
  "Port of data_structures/heap/heap_generic.py.
  Keyed max-heap of [item keyed-value] pairs with update/delete by item.
  State is {:arr :pos-map :size :key}.")

(defn make-heap
  "Empty heap with optional KEY function (default identity)."
  ([] (make-heap identity))
  ([key] {:arr [] :pos-map {} :size 0 :key (or key identity)}))

(defn- heap-parent [i]
  (when (pos? i) (int (/ (dec i) 2))))

(defn- heap-left [{:keys [size]} i]
  (let [l (int (inc (* 2 i)))]
    (when (and (pos? l) (< l size)) l)))

(defn- heap-right [{:keys [size]} i]
  (let [r (int (+ (* 2 i) 2))]
    (when (and (pos? r) (< r size)) r)))

(defn- heap-swap [{:keys [arr pos-map] :as h} i j]
  (-> h
      (assoc-in [:pos-map (nth (nth arr i) 0)] (get pos-map (nth (nth arr j) 0)))
      (assoc-in [:pos-map (nth (nth arr j) 0)] (get pos-map (nth (nth arr i) 0)))
      (assoc :arr (assoc arr i (nth arr j) j (nth arr i)))))

(defn- cmp-less? [{:keys [arr]} i j]
  (< (nth (nth arr i) 1) (nth (nth arr j) 1)))

(defn- valid-parent [h i]
  (let [left (heap-left h i)
        right (heap-right h i)
        vp (if (and left (not (cmp-less? h left i))) left i)]
    (if (and right (not (cmp-less? h right vp))) right vp)))

(defn- heapify-up [h index]
  (loop [h h index index parent (heap-parent index)]
    (if (and parent (not (cmp-less? h index parent)))
      (recur (heap-swap h index parent) parent (heap-parent parent))
      h)))

(defn- heapify-down [h index]
  (loop [h h index index]
    (let [vp (valid-parent h index)]
      (if (= vp index)
        h
        (recur (heap-swap h index vp) vp)))))

(defn update-item
  "Update ITEM's value; returns the new heap."
  [{:keys [pos-map key] :as h} item item-value]
  (if-not (contains? pos-map item)
    h
    (let [index (get pos-map item)
          h (assoc-in h [:arr index] [item (key item-value)])]
      (-> h (heapify-up index) (heapify-down index)))))

(defn delete-item
  "Delete ITEM; returns the new heap."
  [{:keys [pos-map arr size] :as h} item]
  (if-not (contains? pos-map item)
    h
    (let [index (get pos-map item)
          h (-> h
                (update :pos-map dissoc item)
                (assoc-in [:arr index] (nth arr (dec size)))
                (assoc-in [:pos-map (nth (nth arr (dec size)) 0)] index)
                (update :size dec))]
      (if (> (:size h) index)
        (-> h (heapify-up index) (heapify-down index))
        h))))

(defn insert-item
  "Insert ITEM with ITEM-VALUE; returns the new heap."
  [{:keys [arr size key] :as h} item item-value]
  (let [h (if (= (count arr) size)
            (update h :arr conj [item (key item-value)])
            (assoc-in h [:arr size] [item (key item-value)]))
        h (-> h
              (assoc-in [:pos-map item] size)
              (update :size inc))]
    (heapify-up h (dec (:size h)))))

(defn get-top
  "Top [item keyed-value] pair, or nil."
  [{:keys [arr size]}]
  (when (pos? size) (nth arr 0)))

(defn extract-top
  "Vector [top-pair new-heap]."
  [h]
  [(get-top h) (if (get-top h) (delete-item h (nth (get-top h) 0)) h)])
