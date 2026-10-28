(ns clojure-algorithms.data-structures.linked-list.swap-nodes
  "Port of data_structures/linked_list/swap_nodes.py.
  Push prepends; swap exchanges the first occurrences' data.")

(defn make-list
  "Empty list."
  []
  [])

(defn push
  "Prepend NEW-DATA; returns the new list."
  [lst new-data]
  (vec (cons new-data lst)))

(defn swap-nodes
  "Swap first occurrences of DATA-1 and DATA-2; returns the new list.
  No-op when equal or missing."
  [lst data-1 data-2]
  (if (= data-1 data-2)
    lst
    (let [i (.indexOf ^java.util.List lst data-1)
          j (.indexOf ^java.util.List lst data-2)]
      (if (or (neg? i) (neg? j))
        lst
        (assoc lst i data-2 j data-1)))))
