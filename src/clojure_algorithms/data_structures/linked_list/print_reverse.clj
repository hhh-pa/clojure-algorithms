(ns clojure-algorithms.data-structures.linked-list.print-reverse
  "Port of data_structures/linked_list/print_reverse.py.
  Lists are head-first vectors.")

(defn make-list
  "Empty list."
  []
  [])

(defn ll-str
  "String form with \" -> \" separators."
  [lst]
  (clojure.string/join " -> " (map str lst)))

(defn append-item
  "Append DATA; returns the new list."
  [lst data]
  (conj (vec lst) data))

(defn extend-list
  "Append ITEMS; returns the new list."
  [lst items]
  (reduce append-item (vec lst) items))

(defn make-linked-list
  "List of ELEMENTS; raises on empty input."
  [elements]
  (let [v (vec elements)]
    (when (empty? v)
      (throw (ex-info "The Elements List is empty" {:type :exception})))
    v))

(defn in-reverse
  "Elements in reverse joined with \" <- \"."
  [lst]
  (clojure.string/join " <- " (map str (reverse lst))))
