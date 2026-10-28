(ns clojure-algorithms.data-structures.linked-list.singly-linked-list
  "Port of data_structures/linked_list/singly_linked_list.py.
  Immutable singly linked list backed by a vector (head first).
  Deletions return [data new-list].")

(defn make-node
  "Create a node map holding DATA."
  [data]
  {:data data :next-node nil})

(defn node->str
  "Render like the Python repr: \"Node(data)\"."
  [node]
  (str "Node(" (:data node) ")"))

(defn- render [item]
  (if (and (map? item) (contains? item :data)) (node->str item) (str item)))

(defn make-list
  "Empty list."
  []
  [])

(defn list-empty?
  "True when the list is empty."
  [lst]
  (empty? lst))

(defn ll-str
  "String form with \" -> \" separators."
  [lst]
  (clojure.string/join " -> " (map render lst)))

(defn ll-nth
  "Item at INDEX."
  [lst index]
  (if (<= 0 index (dec (count lst)))
    (nth lst index)
    (throw (ex-info "list index out of range." {:type :value-error}))))

(defn ll-assoc
  "Set INDEX to DATA; returns the new list."
  [lst index data]
  (if (<= 0 index (dec (count lst)))
    (assoc lst index data)
    (throw (ex-info "list index out of range." {:type :value-error}))))

(defn insert-nth
  "Insert DATA at INDEX; returns the new list."
  [lst index data]
  (if (<= 0 index (count lst))
    (vec (concat (subvec lst 0 index) [data] (subvec lst index)))
    (throw (ex-info "list index out of range" {:type :index-error}))))

(defn insert-head
  "Prepend DATA; returns the new list."
  [lst data]
  (insert-nth lst 0 data))

(defn insert-tail
  "Append DATA; returns the new list."
  [lst data]
  (insert-nth lst (count lst) data))

(defn print-list
  "Print the list."
  [lst]
  (println (ll-str lst)))

(defn delete-nth
  "Delete INDEX; returns [data new-list]."
  [lst index]
  (if (<= 0 index (dec (count lst)))
    [(nth lst index) (vec (concat (subvec lst 0 index)
                                  (subvec lst (inc index))))]
    (throw (ex-info "List index out of range." {:type :index-error}))))

(defn delete-head
  "Delete the head; returns [data new-list]."
  [lst]
  (delete-nth lst 0))

(defn delete-tail
  "Delete the tail; returns [data new-list]."
  [lst]
  (delete-nth lst (dec (count lst))))

(defn ll-reverse
  "Reversed list."
  [lst]
  (vec (reverse lst)))
