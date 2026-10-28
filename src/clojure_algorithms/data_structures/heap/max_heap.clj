(ns clojure-algorithms.data-structures.heap.max-heap
  "Port of data_structures/heap/max_heap.py.
  1-indexed binary max-heap. State is {:heap [...] :size n}.")

(defn make-binary-heap
  "Empty binary max-heap."
  []
  {:heap [0] :size 0})

(defn- swap-up [{:keys [heap] :as h} i]
  (loop [hp heap i i temp (nth heap i)]
    (if (pos? (quot i 2))
      (let [p (quot i 2)]
        (recur (if (> (nth hp i) (nth hp p))
                 (assoc hp i (nth hp p) p temp)
                 hp)
               p temp))
      (assoc h :heap hp))))

(defn heap-insert
  "Insert VALUE; returns the new heap."
  [h value]
  (let [h (-> h (update :heap conj value) (update :size inc))]
    (swap-up h (:size h))))

(defn- swap-down [{:keys [heap size] :as h} i]
  (loop [hp heap i i]
    (if (>= size (* 2 i))
      (let [bigger (cond
                     (> (inc (* 2 i)) size) (* 2 i)
                     (> (nth hp (* 2 i)) (nth hp (inc (* 2 i)))) (* 2 i)
                     :else (inc (* 2 i)))
            hp (if (< (nth hp i) (nth hp bigger))
                 (assoc hp i (nth hp bigger) bigger (nth hp i))
                 hp)]
        (recur hp bigger))
      (assoc h :heap hp))))

(defn heap-pop
  "Vector [max-value new-heap]."
  [{:keys [heap size] :as h}]
  (let [max-value (nth heap 1)
        h (-> h
              (assoc-in [:heap 1] (nth heap size))
              (update :size dec)
              (update :heap pop))]
    [max-value (swap-down h 1)]))

(defn heap-list
  "Heap contents without the sentinel."
  [{:keys [heap]}]
  (subvec heap 1))

(defn heap-len
  "Number of elements."
  [h]
  (:size h))
