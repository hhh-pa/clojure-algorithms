(ns clojure-algorithms.data-structures.linked-list.reverse-k-group
  "Port of data_structures/linked_list/reverse_k_group.py.
  Lists are head-first vectors.")

(defn make-list
  "List of INTS."
  [ints]
  (vec ints))

(defn ll-str
  "String form with \" -> \" separators."
  [lst]
  (clojure.string/join " -> " (map str lst)))

(defn append-item
  "Append DATA; returns the new list."
  [lst data]
  (conj (vec lst) data))

(defn reverse-k-nodes
  "Reverse every complete GROUP-SIZE block; returns the new list."
  [lst group-size]
  (if (< (count lst) 2)
    (vec lst)
    (let [n (* group-size (quot (count lst) group-size))]
      (vec (concat (mapcat reverse (partition group-size (take n lst)))
                   (drop n lst))))))
