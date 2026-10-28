(ns clojure-algorithms.data-structures.stacks.stack-using-two-queues
  "Port of data_structures/stacks/stack_using_two_queues.py.
  Stack from two queues, functionally: {:q1 [...] :q2 [...]}.
  push appends; pop rotates; peek returns the top or nil.")

(defn make-stack
  "Empty stack."
  []
  {:q1 clojure.lang.PersistentQueue/EMPTY
   :q2 clojure.lang.PersistentQueue/EMPTY})

(defn stack-push
  "Push ITEM; returns the new stack."
  [st item]
  (update st :q1 conj item))

(defn stack-peek
  "Top item, or nil."
  [{:keys [q1 q2]}]
  (cond
    (seq q1) (peek (vec q1))
    (seq q2) (peek (vec q2))
    :else nil))

(defn stack-pop
  "Pop; returns [value new-stack]."
  [{:keys [q1 q2] :as st}]
  (if (and (empty? q1) (empty? q2))
    (throw (ex-info "pop from an empty deque" {:type :index-error}))
    (let [[main other] (if (seq q1) [:q1 :q2] [:q2 :q1])
          items (vec (get st main))
          value (peek items)
          moved (reduce conj (get st other) (pop items))]
      [value (assoc st main clojure.lang.PersistentQueue/EMPTY
                    other moved)])))
