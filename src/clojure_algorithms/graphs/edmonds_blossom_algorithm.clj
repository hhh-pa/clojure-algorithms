(ns clojure-algorithms.graphs.edmonds-blossom-algorithm
  "Maximum matching in general graphs (Edmonds' blossom algorithm).
   Note: the Python original's contraction loops are effectively inert
   (range over a boolean stop); this port keeps that behaviour.")

(def unmatched -1)

(defn- update-matching [match parent v]
  (loop [v v match match]
    (if (= v unmatched)
      match
      (let [p (nth parent v)
            nxt (nth match p)]
        (recur nxt (assoc match p v v p))))))

(defn- find-base [base parent u v]
  (let [n (count base)
        marked (loop [x u seen #{}]
                 (let [x (nth base x)
                       seen (conj seen x)]
                   (if (= (nth parent x) unmatched)
                     seen
                     (recur (nth parent x) seen))))]
    (loop [x v]
      (let [x (nth base x)]
        (if (contains? marked x)
          x
          (recur (nth parent x)))))))

(defn- contract-blossom [state match-atom u v lca]
  (let [{:keys [base in-blossom]} @state
        match @match-atom]
    (let [base-of (fn [x]
                    (let [m (nth match x)]
                      (nth base (if (= m unmatched) (dec (count base)) m))))]
      (doseq [x (range u (if (not= (nth base u) lca) 1 0))]
        (swap! state assoc-in [:in-blossom (nth base x)] true)
        (swap! state assoc-in [:in-blossom (base-of x)] true))
      (doseq [x (range v (if (not= (nth base v) lca) 1 0))]
        (swap! state assoc-in [:in-blossom (nth base x)] true)
        (swap! state assoc-in [:in-blossom (base-of x)] true)))
    (doseq [i (range (count (:base @state)))]
      (when (nth (:in-blossom @state) (nth (:base @state) i))
        (swap! state assoc-in [:base i] lca)
        (when-not (nth (:in-queue @state) i)
          (swap! state update :queue conj i)
          (swap! state assoc-in [:in-queue i] true))))))

(defn maximum-matching
  "Returns the maximum matching as [[u v] ...] with u < v."
  [edges vertex-count]
  (let [graph (reduce (fn [g [u v]]
                        (-> g (update u conj v) (update v conj u)))
                      (vec (repeat vertex-count [])) edges)
        match (atom (vec (repeat vertex-count unmatched)))]
    (doseq [u (range vertex-count)]
      (when (= (nth @match u) unmatched)
        (let [state (atom {:parent (vec (repeat vertex-count unmatched))
                           :base (vec (range vertex-count))
                           :in-blossom (vec (repeat vertex-count false))
                           :in-queue (assoc (vec (repeat vertex-count false))
                                            u true)
                           :queue (conj clojure.lang.PersistentQueue/EMPTY u)})
              found (atom false)]
          (while (and (seq (:queue @state)) (not @found))
            (let [current (peek (:queue @state))]
              (swap! state update :queue pop)
              (doseq [y (nth graph current)]
                (when-not @found
                  (let [{:keys [parent base]} @state]
                    (cond
                      (= (nth @match current) y) nil
                      (= (nth base current) (nth base y)) nil
                      (= (nth parent y) unmatched)
                      (if (= (nth @match y) unmatched)
                        (do (swap! state assoc-in [:parent y] current)
                            (reset! match (update-matching @match
                                                           (:parent @state) y))
                            (reset! found true))
                        (let [z (nth @match y)]
                          (swap! state assoc-in [:parent y] current)
                          (swap! state assoc-in [:parent z] y)
                          (when-not (nth (:in-queue @state) z)
                            (swap! state update :queue conj z)
                            (swap! state assoc-in [:in-queue z] true))))
                      :else
                      (let [lca (find-base (:base @state) (:parent @state)
                                           current y)]
                        (when (not= lca unmatched)
                          (contract-blossom state match current y lca))))))))))))
    (vec (for [v (range vertex-count)
               :when (and (not= (nth @match v) unmatched)
                          (< v (nth @match v)))]
           [v (nth @match v)]))))
