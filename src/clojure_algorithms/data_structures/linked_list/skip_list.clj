(ns clojure-algorithms.data-structures.linked-list.skip-list
  "Port of data_structures/linked_list/skip_list.py.
  Probabilistic skip-list map with stringable structure.
  State is {:nodes {key {:value :forward [keys]}} :head-forward [...]
  :level :p :max-level}; :head-forward holds level forwards.")

(defn make-node
  "Create a skip node map."
  ([key value] (make-node key value []))
  ([key value forward] {:key key :value value :forward (vec forward)}))

(defn node->str
  "Render like the Python repr."
  [node]
  (str "Node(" (:key node) ": " (:value node) ")"))

(defn make-skip-list
  "Empty skip list."
  ([] (make-skip-list 0.5 16))
  ([p max-level] {:nodes {} :head-forward [] :level 0 :p p
                  :max-level max-level}))

(defn random-level
  "Random level in [1, max-level]."
  [{:keys [p max-level]}]
  (loop [level 1]
    (if (and (< (rand) p) (< level max-level))
      (recur (inc level))
      level)))

(defn- forwards-of [{:keys [nodes head-forward]} key]
  (if (= key :head) head-forward (get-in nodes [key :forward])))

(defn- locate-node [{:keys [level] :as sl} key]
  (loop [i (dec level) node :head update []]
    (if (neg? i)
      [node (vec (reverse update))]
      (let [node (loop [n node]
                   (let [fw (forwards-of sl n)]
                     (if (and (< i (count fw))
                              (some? (nth fw i))
                              (< (compare (nth fw i) key) 0))
                       (recur (nth fw i))
                       n)))]
        (recur (dec i) node (conj update node))))))

(defn skip-find
  "Value for KEY, or nil."
  [sl key]
  (get-in sl [:nodes key :value]))

(declare insert-with-level)

(defn skip-insert
  "Insert KEY/VALUE (overwriting); returns the new skip list."
  [sl key value]
  (if (contains? (:nodes sl) key)
    (assoc-in sl [:nodes key :value] value)
    (insert-with-level sl key value (random-level sl))))

(defn- rebuild-after-insert
  "Fix UP-chain forwards after inserting KEY at NEW-LEVEL."
  [sl update-vec key new-level]
  (reduce (fn [sl i]
            (let [u (nth update-vec i)]
              (if (= u :head)
                (update sl :head-forward
                        (fn [fw]
                          (let [fw (vec (concat fw (repeat (max 0 (- (inc i)
                                                                     (count fw)))
                                                           nil)))]
                            (assoc fw i key))))
                (update-in sl [:nodes u :forward]
                           (fn [fw]
                             (let [fw (vec (concat fw (repeat (max 0 (- (inc i)
                                                                        (count fw)))
                                                              nil)))]
                               (assoc fw i key)))))))
          sl (range new-level)))

(defn- insert-with-level [{:keys [level] :as sl} key value new-level]
  (let [[_ update-vec] (locate-node sl key)
        update (vec (concat update-vec (repeat (max 0 (- new-level (count update-vec)))
                                               :head)))
        sl (assoc sl :level (max level new-level))
        succs (mapv (fn [i]
                      (let [fw (forwards-of sl (nth update i))]
                        (when (< i (count fw)) (nth fw i))))
                    (range new-level))
        sl (assoc-in sl [:nodes key] (make-node key value succs))]
    (rebuild-after-insert sl update key new-level)))

(defn skip-insert*
  "Insert with an explicit level (deterministic helper)."
  [sl key value new-level]
  (if (contains? (:nodes sl) key)
    (assoc-in sl [:nodes key :value] value)
    (insert-with-level sl key value new-level)))

(defn skip-delete
  "Delete KEY; returns the new skip list."
  [sl key]
  (if-not (contains? (:nodes sl) key)
    sl
    (let [[_ update-vec] (locate-node sl key)
          node-level (count (get-in sl [:nodes key :forward]))
          sl (reduce (fn [sl i]
                       (let [u (nth update-vec i)
                             succ (nth (get-in sl [:nodes key :forward]) i nil)]
                         (if (= u :head)
                           (update sl :head-forward
                                   (fn [fw]
                                     (if (and (< i (count fw))
                                              (= (nth fw i) key))
                                       (assoc fw i succ)
                                       fw)))
                           (update-in sl [:nodes u :forward]
                                      (fn [fw]
                                        (if (and (< i (count fw))
                                                 (= (nth fw i) key))
                                          (assoc fw i succ)
                                          fw))))))
                     sl (range (count update-vec)))
          sl (update sl :nodes dissoc key)
          trim (fn [fw] (vec (reverse (drop-while nil? (reverse fw)))))
          sl (update sl :head-forward trim)
          sl (update sl :nodes
                     (fn [nodes]
                       (into {} (map (fn [[k n]]
                                       [k (update n :forward trim)]))
                             nodes)))]
      (assoc sl :level (count (:head-forward sl))))))

(defn skip-keys
  "Keys in order."
  [{:keys [nodes head-forward]}]
  (loop [fw head-forward out []]
    (if (or (empty? fw) (nil? (first fw)))
      out
      (let [k (first fw)]
        (recur (get-in nodes [k :forward]) (conj out k))))))

(defn skip->str
  "Render like the Python str (level line plus forward diagram)."
  [{:keys [level head-forward nodes] :as sl}]
  (let [items (skip-keys sl)]
    (if (empty? items)
      (str "SkipList(level=" level ")")
      (let [label-size (+ (max 4 (apply max (map #(count (str %)) items))) 4)
            pad (fn [s] (let [s (str s)]
                          (str s (apply str (repeat (- label-size (count s))
                                                    \-)))))]
        (clojure.string/join "\n"
                             (concat [(str "SkipList(level=" level ")")
                                      (str (pad "[root]") (clojure.string/join " "
                                                                               (repeat (count head-forward) "*")))
                                      (str (apply str (repeat label-size \ ))
                                           (clojure.string/join " "
                                                                (repeat (count head-forward) "|")))]
                                     (mapcat (fn [k]
                                               [(str (pad (str "[" k "]"))
                                                     (clojure.string/join " "
                                                                          (repeat (count head-forward) "|")))
                                                (str (apply str (repeat label-size \ ))
                                                     (clojure.string/join " "
                                                                          (repeat (count head-forward) "|")))])
                                             items)
                                     [(str (pad "None") (clojure.string/join " "
                                                                             (repeat (count head-forward) "*")))]))))))
