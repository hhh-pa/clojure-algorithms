(ns clojure-algorithms.data-structures.linked-list.has-loop
  "Port of data_structures/linked_list/has_loop.py.
  Loop detection via a visited set over an id-based node store
  {:nodes {id {:data :next}} :head id :next-id n}.")

(defn make-list
  "Empty list."
  []
  {:nodes {} :head nil :next-id 0 :tail nil})

(defn add-node
  "Append DATA; returns the new list."
  [{:keys [nodes next-id tail] :as lst} data]
  (let [id next-id
        nodes (assoc nodes id {:data data :next nil})
        nodes (if (some? tail) (assoc-in nodes [tail :next] id) nodes)]
    (assoc lst :nodes nodes :next-id (inc id) :tail id
           :head (or (:head lst) id))))

(defn link-nodes
  "Point node FROM-ID at TO-ID (builds loops); returns the new list."
  [lst from-id to-id]
  (assoc-in lst [:nodes from-id :next] to-id))

(defn to-vec
  "Data values; throws ContainsLoopError-style ex-info on a loop."
  [{:keys [nodes head]}]
  (loop [id head seen #{} out []]
    (cond
      (nil? id) out
      (contains? seen id)
      (throw (ex-info "ContainsLoopError" {:type :contains-loop-error}))
      :else (recur (get-in nodes [id :next]) (conj seen id)
                   (conj out (get-in nodes [id :data]))))))

(defn has-loop?
  "True when the list contains a loop."
  [lst]
  (try (do (to-vec lst) false)
       (catch clojure.lang.ExceptionInfo e
         (= :contains-loop-error (:type (ex-data e))))))
