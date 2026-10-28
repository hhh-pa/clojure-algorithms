(ns clojure-algorithms.data-structures.stacks.stack
  "Port of data_structures/stacks/stack.py.
  Bounded stack. State is {:items [...] :limit n}.
  push returns the new stack; pop/peek return values (peek keeps state).")

(defn make-stack
  "Stack with LIMIT slots (default 10)."
  ([] (make-stack 10))
  ([limit] {:items [] :limit limit}))

(defn stack-bool
  "True when non-empty."
  [{:keys [items]}]
  (boolean (seq items)))

(defn- render [v]
  (if (or (string? v) (char? v)) (str "'" v "'") (str v)))

(defn stack-str
  "Render like the Python str."
  [{:keys [items]}]
  (str "[" (clojure.string/join ", " (map render items)) "]"))

(defn stack-push
  "Push DATA; returns the new stack."
  [{:keys [items limit] :as st} data]
  (if (>= (count items) limit)
    (throw (ex-info "StackOverflowError" {:type :stack-overflow-error}))
    (update st :items conj data)))

(defn stack-pop
  "Vector [value new-stack]."
  [{:keys [items] :as st}]
  (if (empty? items)
    (throw (ex-info "StackUnderflowError" {:type :stack-underflow-error}))
    [(peek items) (update st :items pop)]))

(defn stack-peek
  "Top value."
  [{:keys [items]}]
  (if (empty? items)
    (throw (ex-info "StackUnderflowError" {:type :stack-underflow-error}))
    (peek items)))

(defn stack-empty?
  "True when empty."
  [{:keys [items]}]
  (empty? items))

(defn stack-full?
  "True when at the limit."
  [{:keys [items limit]}]
  (= (count items) limit))

(defn stack-size
  "Number of items."
  [{:keys [items]}]
  (count items))

(defn stack-contains?
  "True when ITEM is present."
  [{:keys [items]} item]
  (boolean (some #(= % item) items)))
