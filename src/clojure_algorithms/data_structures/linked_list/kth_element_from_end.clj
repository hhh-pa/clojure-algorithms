(ns clojure-algorithms.data-structures.linked-list.kth-element-from-end
  "Port of data_structures/linked_list/kth_element_from_end.py.
  Lists are head-first vectors; kth queries are 1-based from the end.")

(defn make-list
  "Empty list."
  []
  [])

(defn ll-str
  "String form with \" --> \" separators."
  [lst]
  (clojure.string/join " --> " (map str lst)))

(defn add-item
  "Insert ITEM at POSITION (default head); returns the new list."
  ([lst item] (add-item lst item 0))
  ([lst item position]
   (when (neg? position)
     (throw (ex-info "Position must be non-negative" {:type :value-error})))
   (when (> position (count lst))
     (throw (ex-info "Out of bounds" {:type :value-error})))
   (vec (concat (subvec lst 0 position) [item] (subvec lst position)))))

(defn kth-element-from-end
  "POSITION-th element from the end (1-based); nil when invalid."
  [lst position]
  (when (and (seq lst) (pos? position) (<= position (count lst)))
    (nth lst (- (count lst) position))))
