(ns clojure-algorithms.cellular-automata.wa-tor
  "Wa-Tor predator/prey simulation over immutable planet states.")

(def width 50)
(def height 50)
(def prey-initial-count 30)
(def prey-reproduction-time 5)
(def predator-initial-count 50)
(def predator-initial-energy 15)
(def predator-food-value 5)
(def predator-reproduction-time 20)
(def max-entities 500)
(def delete-unbalanced 50)

(def ^:private id-counter (atom 0))
(defn- next-id [] (swap! id-counter inc))

(defn make-entity
  "Creates a prey or predator entity map at [row col]."
  [prey? coords]
  {:id (next-id)
   :prey (boolean prey?)
   :coords coords
   :repro (if prey? prey-reproduction-time predator-reproduction-time)
   :energy (when-not prey? predator-initial-energy)
   :alive true})

(defn reset-reproduction-time
  "Restores the entity reproduction countdown."
  [entity]
  (assoc entity :repro (if (:prey entity)
                         prey-reproduction-time
                         predator-reproduction-time)))

(defn empty-planet
  "Creates an empty w x h planet state."
  [w h]
  {:width w :height h
   :grid (vec (repeat h (vec (repeat w nil))))})

(defn- random-empty-cell
  [state]
  (loop []
    (let [r (rand-int (:height state)) c (rand-int (:width state))]
      (if (nil? (get-in (:grid state) [r c])) [r c] (recur)))))

(defn add-entity
  "Places a new prey/predator on a random empty cell."
  [state prey?]
  (let [cell (random-empty-cell state)]
    (assoc-in state [:grid (first cell) (second cell)]
              (make-entity prey? cell))))

(defn make-planet
  "Creates a w x h planet populated with initial entities."
  [w h]
  (as-> (empty-planet w h) s
    (reduce (fn [st _] (add-entity st true)) s (range prey-initial-count))
    (reduce (fn [st _] (add-entity st false)) s (range predator-initial-count))))

(defn get-entities
  "Returns every entity on the planet."
  [state]
  (vec (remove nil? (apply concat (:grid state)))))

(defn balance-predators-and-prey
  "Purges entities from the overpopulated side when crowded."
  [state]
  (let [entities (shuffle (get-entities state))]
    (if (< (count entities) (- max-entities (/ max-entities 10)))
      state
      (let [prey (filter :prey entities)
            predators (remove :prey entities)
            purge (take delete-unbalanced
                        (if (> (count prey) (count predators)) prey predators))]
        (reduce (fn [st e]
                  (assoc-in st [:grid (first (:coords e)) (second (:coords e))] nil))
                state purge)))))

(defn get-surrounding-prey
  "Returns prey entities adjacent (N/S/E/W) to the given predator."
  [state entity]
  (let [[row col] (:coords entity)]
    (vec (for [[r c] [[(dec row) col] [(inc row) col]
                      [row (dec col)] [row (inc col)]]
               :when (and (<= 0 r (dec (:height state)))
                          (<= 0 c (dec (:width state))))
               :let [ent (get-in (:grid state) [r c])]
               :when (and ent (:prey ent))]
           ent))))

(def ^:private dir-delta {"N" [-1 0] "S" [1 0] "W" [0 -1] "E" [0 1]})

(defn move-and-reproduce
  "Moves the entity at coords to a free neighbour, maybe reproducing."
  [state coords direction-orders]
  (let [[row col] coords
        entity (get-in (:grid state) [row col])]
    (if (nil? entity)
      state
      (let [target (some (fn [d]
                           (let [[dr dc] (dir-delta d)
                                 r (+ row dr) c (+ col dc)]
                             (when (and (<= 0 r (dec (:height state)))
                                        (<= 0 c (dec (:width state)))
                                        (nil? (get-in (:grid state) [r c])))
                               [r c])))
                         direction-orders)]
        (if (nil? target)
          (update-in state [:grid row col :repro] dec)
          (let [[r c] target
                moved (assoc entity :coords [r c])
                st (-> state
                       (assoc-in [:grid r c] moved)
                       (assoc-in [:grid row col] nil))]
            (if (and (<= (:repro entity) 0)
                     (< (count (get-entities st)) max-entities))
              (-> st
                  (assoc-in [:grid row col] (make-entity (:prey entity) [row col]))
                  (assoc-in [:grid r c] (reset-reproduction-time moved)))
              (update-in st [:grid r c :repro] dec))))))))

(defn perform-prey-actions
  "Moves a prey entity and maybe reproduces."
  [state entity direction-orders]
  (move-and-reproduce state (:coords entity) direction-orders))

(defn perform-predator-actions
  "Runs one predator turn: eat adjacent prey or move, then lose energy."
  [state entity prey-coords direction-orders]
  (let [[row col] (:coords entity)
        live (get-in (:grid state) [row col])]
    (cond
      (or (nil? live) (not= (:id live) (:id entity))) state
      (zero? (:energy live))
      (assoc-in state [:grid row col] nil)
      prey-coords
      (let [[pr pc] prey-coords
            moved (assoc live :coords prey-coords
                         :energy (+ (:energy live) predator-food-value))]
        (-> state
            (assoc-in [:grid pr pc] moved)
            (assoc-in [:grid row col] nil)
            (update-in [:grid pr pc :energy] dec)))
      :else
      (let [st (move-and-reproduce state [row col] direction-orders)
            [nr nc] (:coords (or (some #(when (= (:id %) (:id entity)) %)
                                       (get-entities st))
                                 entity))]
        (if (get-in (:grid st) [nr nc])
          (update-in st [:grid nr nc :energy] dec)
          st)))))

(defn run-step
  "Advances the planet by one chronon."
  [state]
  (let [entities (shuffle (get-entities state))]
    (balance-predators-and-prey
     (reduce (fn [st entity]
               (let [[r c] (:coords entity)
                     live (get-in (:grid st) [r c])]
                 (if (or (nil? live) (not= (:id live) (:id entity)))
                   st
                   (let [dirs (shuffle ["N" "E" "S" "W"])]
                     (if (:prey live)
                       (perform-prey-actions st live dirs)
                       (let [prey (get-surrounding-prey st live)
                             target (when (seq prey)
                                      (:coords (rand-nth prey)))]
                         (perform-predator-actions st live target dirs)))))))
             state entities))))

(defn run-simulation
  "Advances the planet by n chronons."
  [state n]
  (nth (iterate run-step state) n))

(defn render-planet
  "Renders the planet as plain text with population counts."
  [state iter-number]
  (let [rows (map (fn [row]
                    (apply str (map (fn [e]
                                      (cond (nil? e) " . "
                                            (:prey e) " # "
                                            :else " x "))
                                    row)))
                  (:grid state))
        entities (get-entities state)
        prey-count (count (filter :prey entities))]
    (str (clojure.string/join "\n" rows)
         "\n\nIteration: " iter-number
         " | Prey count: " prey-count
         " | Predator count: " (- (count entities) prey-count) " |")))
