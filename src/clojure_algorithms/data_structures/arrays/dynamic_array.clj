(ns clojure-algorithms.data-structures.arrays.dynamic-array
  "Port of data_structures/arrays/dynamic_array.py.
  An immutable dynamic array: {:size n :capacity c :data [...]}.")

(defn make-dynamic-array
  "Create an empty dynamic array with capacity 1."
  []
  {:size 0 :capacity 1 :data [nil]})

(defn darray-append
  "Append ITEM, doubling capacity when full."
  [arr item]
  (let [{:keys [size capacity data]} arr
        arr (if (= size capacity)
              {:size size :capacity (* 2 capacity)
               :data (vec (concat data (repeat capacity nil)))}
              arr)]
    {:size (inc (:size arr)) :capacity (:capacity arr)
     :data (assoc (:data arr) (:size arr) item)}))

(defn darray-resize
  "Resize ARR to NEW-CAPACITY."
  [arr new-capacity]
  {:size (:size arr) :capacity new-capacity
   :data (vec (concat (subvec (:data arr) 0 (:size arr))
                      (repeat (- new-capacity (:size arr)) nil)))})

(defn darray-get
  "Item at INDEX; throws when out of range."
  [arr index]
  (if (<= 0 index (dec (:size arr)))
    (nth (:data arr) index)
    (throw (ex-info "index out of range" {:type :index-error}))))

(defn darray-set
  "Set INDEX to VALUE; throws when out of range."
  [arr index value]
  (if (<= 0 index (dec (:size arr)))
    (assoc arr :data (assoc (:data arr) index value))
    (throw (ex-info "index out of range" {:type :index-error}))))

(defn darray-len
  "Number of elements in ARR."
  [arr]
  (:size arr))

(defn darray-items
  "Vector of the filled elements of ARR."
  [arr]
  (subvec (:data arr) 0 (:size arr)))
