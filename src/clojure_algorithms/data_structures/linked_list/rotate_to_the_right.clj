(ns clojure-algorithms.data-structures.linked-list.rotate-to-the-right
  "Port of data_structures/linked_list/rotate_to_the_right.py.
  Lists are head-first vectors.")

(defn insert-node
  "Append DATA; returns the new list."
  [lst data]
  (if (nil? lst) [data] (conj (vec lst) data)))

(defn print-linked-list
  "Print the list with \"->\" separators."
  [lst]
  (when (seq lst)
    (println (clojure.string/join "->" (map str lst)))))

(defn ll-str
  "String form with \"->\" separators."
  [lst]
  (clojure.string/join "->" (map str lst)))

(defn rotate-to-the-right
  "Rotate right by PLACES; returns the new list."
  [lst places]
  (when (empty? lst)
    (throw (ex-info "The linked list is empty." {:type :value-error})))
  (let [n (count lst)
        places (mod places n)]
    (if (zero? places)
      (vec lst)
      (vec (concat (subvec (vec lst) (- n places)) (subvec (vec lst) 0 (- n places)))))))
