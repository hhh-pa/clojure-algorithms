(ns clojure-algorithms.data-structures.linked-list.doubly-linked-list
  "Port of data_structures/linked_list/doubly_linked_list.py.
  Immutable doubly linked list backed by a vector (head first).
  Deletions return [data new-list].")

(defn make-list
  "Empty list."
  []
  [])

(defn list-empty?
  "True when the list is empty."
  [lst]
  (empty? lst))

(defn ll-str
  "String form with \"->\" separators."
  [lst]
  (clojure.string/join "->" (map str lst)))

(defn insert-at-nth
  "Insert DATA at INDEX; returns the new list."
  [lst index data]
  (if (<= 0 index (count lst))
    (vec (concat (subvec lst 0 index) [data] (subvec lst index)))
    (throw (ex-info "list index out of range" {:type :index-error}))))

(defn insert-at-head
  "Prepend DATA; returns the new list."
  [lst data]
  (insert-at-nth lst 0 data))

(defn insert-at-tail
  "Append DATA; returns the new list."
  [lst data]
  (insert-at-nth lst (count lst) data))

(defn delete-at-nth
  "Delete INDEX; returns [data new-list]."
  [lst index]
  (if (<= 0 index (dec (count lst)))
    [(nth lst index) (vec (concat (subvec lst 0 index)
                                  (subvec lst (inc index))))]
    (throw (ex-info "list index out of range" {:type :index-error}))))

(defn delete-head
  "Delete the head; returns [data new-list]."
  [lst]
  (delete-at-nth lst 0))

(defn delete-tail
  "Delete the tail; returns [data new-list]."
  [lst]
  (delete-at-nth lst (dec (count lst))))

(defn delete-value
  "Delete the first DATA; returns [data new-list]."
  [lst data]
  (when (empty? lst)
    (throw (ex-info "No data matching given value" {:type :value-error})))
  (let [idx (.indexOf ^java.util.List (vec lst) data)]
    (if (neg? idx)
      (throw (ex-info "No data matching given value" {:type :value-error}))
      (let [[_ new-lst] (delete-at-nth lst idx)]
        [data new-lst]))))
