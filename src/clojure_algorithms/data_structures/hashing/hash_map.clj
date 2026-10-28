(ns clojure-algorithms.data-structures.hashing.hash-map
  "Port of data_structures/hashing/hash_map.py.
  Open-addressing hash map with linear probing and tombstones.
  State is {:initial-block-size :buckets :capacity-factor :len};
  buckets hold nil, {:key :val} or :deleted.")

(defn make-hash-map
  "Hash map with INITIAL-BLOCK-SIZE buckets."
  ([] (make-hash-map 8 0.75))
  ([initial-block-size] (make-hash-map initial-block-size 0.75))
  ([initial-block-size capacity-factor]
   (when-not (< 0.0 capacity-factor 1.0)
     (throw (ex-info "capacity factor must be in (0, 1)"
                     {:type :assertion-error})))
   {:initial-block-size initial-block-size
    :buckets (vec (repeat initial-block-size nil))
    :capacity-factor capacity-factor
    :len 0}))

(defn- bucket-index [{:keys [buckets]} key]
  (mod (hash key) (count buckets)))

(defn next-index
  "Index following IND."
  [{:keys [buckets]} ind]
  (mod (inc ind) (count buckets)))

(defn- bucket-seq [hm key]
  (take (count (:buckets hm)) (iterate #(next-index hm %) (bucket-index hm key))))

(defn hm-len
  "Number of items."
  [hm]
  (:len hm))

(defn- full? [{:keys [buckets capacity-factor] :as hm}]
  (>= (hm-len hm) (int (* (count buckets) capacity-factor))))

(defn- sparse? [{:keys [buckets initial-block-size capacity-factor] :as hm}]
  (and (> (count buckets) initial-block-size)
       (< (hm-len hm) (/ (* (count buckets) capacity-factor) 2))))

(defn- try-set [{:keys [buckets] :as hm} ind key val]
  (let [stored (nth buckets ind)]
    (cond
      (or (nil? stored) (= stored :deleted))
      [(-> hm
           (assoc-in [:buckets ind] {:key key :val val})
           (update :len inc)) true]
      (= (:key stored) key)
      [(assoc-in hm [:buckets ind :val] val) true]
      :else [hm false])))

(defn- add-item [hm key val]
  (loop [hm hm inds (bucket-seq hm key)]
    (if (empty? inds)
      hm
      (let [[hm done?] (try-set hm (first inds) key val)]
        (if done? hm (recur hm (rest inds)))))))

(defn- resize [hm new-size]
  (let [items (filter #(and (map? %) (:key %)) (:buckets hm))]
    (reduce (fn [hm {:keys [key val]}] (add-item hm key val))
            (assoc hm :buckets (vec (repeat new-size nil)) :len 0)
            items)))

(defn hm-set
  "Associate KEY to VAL; returns the new map."
  [hm key val]
  (add-item (if (full? hm) (resize hm (* 2 (count (:buckets hm)))) hm)
            key val))

(defn hm-del
  "Remove KEY; returns the new map. Raises on missing keys."
  [{:keys [buckets] :as hm} key]
  (let [hm (loop [hm hm inds (bucket-seq hm key)]
             (if (empty? inds)
               (throw (ex-info (str "KeyError: " key) {:type :key-error}))
               (let [ind (first inds)
                     item (nth (:buckets hm) ind)]
                 (cond
                   (nil? item)
                   (throw (ex-info (str "KeyError: " key)
                                   {:type :key-error}))
                   (= item :deleted) (recur hm (rest inds))
                   (= (:key item) key)
                   (-> hm
                       (assoc-in [:buckets ind] :deleted)
                       (update :len dec))
                   :else (recur hm (rest inds))))))]
    (if (sparse? hm)
      (resize hm (quot (count (:buckets hm)) 2))
      hm)))

(defn hm-get
  "Value for KEY. Raises on missing keys."
  [{:keys [buckets] :as hm} key]
  (loop [inds (bucket-seq hm key)]
    (if (empty? inds)
      (throw (ex-info (str "KeyError: " key) {:type :key-error}))
      (let [item (nth buckets (first inds))]
        (cond
          (nil? item) (throw (ex-info (str "KeyError: " key)
                                      {:type :key-error}))
          (= item :deleted) (recur (rest inds))
          (= (:key item) key) (:val item)
          :else (recur (rest inds)))))))

(defn hm-keys
  "Key sequence in bucket order."
  [{:keys [buckets]}]
  (seq (map :key (filter #(and (map? %) (:key %)) buckets))))

(defn hm-repr
  "Render like the Python repr."
  [{:keys [buckets]}]
  (str "HashMap("
       (clojure.string/join ", "
                            (map (fn [{:keys [key val]}] (str key ": " val))
                                 (filter #(and (map? %) (:key %)) buckets)))
       ")"))
