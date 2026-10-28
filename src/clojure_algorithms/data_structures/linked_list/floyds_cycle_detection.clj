(ns clojure-algorithms.data-structures.linked-list.floyds-cycle-detection
  "Port of data_structures/linked_list/floyds_cycle_detection.py.
  Floyd's cycle detection over an id-based node store
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
  "Point node FROM-ID at TO-ID (builds cycles); returns the new list."
  [lst from-id to-id]
  (assoc-in lst [:nodes from-id :next] to-id))

(defn to-vec
  "Data values, stopping at repeats."
  [{:keys [nodes head]}]
  (loop [id head seen #{} out []]
    (if (or (nil? id) (contains? seen id))
      out
      (recur (get-in nodes [id :next]) (conj seen id)
             (conj out (get-in nodes [id :data]))))))

(defn detect-cycle
  "True when the list contains a cycle (tortoise and hare)."
  [{:keys [nodes head]}]
  (if (nil? head)
    false
    (loop [slow head fast head]
      (let [fast-next (when (some? fast) (get-in nodes [fast :next]))
            fast-next2 (when (some? fast-next)
                         (get-in nodes [fast-next :next]))
            slow-next (when (some? slow) (get-in nodes [slow :next]))]
        (if (or (nil? fast-next) (nil? fast-next2))
          false
          (if (= slow-next fast-next2)
            true
            (recur slow-next fast-next2)))))))
