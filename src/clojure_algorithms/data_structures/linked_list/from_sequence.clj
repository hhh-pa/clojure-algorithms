(ns clojure-algorithms.data-structures.linked-list.from-sequence
  "Port of data_structures/linked_list/from_sequence.py.
  Builds {:data :next} chains from sequences.")

(defn make-node
  "Create a chain node with DATA and optional NEXT."
  ([data] (make-node data nil))
  ([data next] {:data data :next next}))

(defn node->str
  "Render the chain: \"<1> ---> <2> ---> <END>\"."
  [node]
  (str (clojure.string/join ""
                            (map (fn [n] (str "<" (:data n) "> ---> "))
                                 (take-while some? (iterate :next node))))
       "<END>"))

(defn make-linked-list
  "Chain over ELEMENTS; raises on empty input."
  [elements]
  (let [v (vec elements)]
    (when (empty? v)
      (throw (ex-info "The Elements List is empty" {:type :value-error})))
    (reduce (fn [nxt x] (make-node x nxt)) nil (reverse v))))
