(ns clojure-algorithms.cellular-automata.nagel-schrekenberg
  "Nagel-Schreckenberg single-lane traffic model on a loop highway.
  -1 marks an empty cell, otherwise the value is the car speed.")

(defn construct-highway
  "Builds the initial highway row. Options: :random-frequency, :random-speed."
  [number-of-cells frequency initial-speed
   & {:keys [random-frequency random-speed max-speed]
      :or {random-frequency false random-speed false max-speed 5}}]
  (let [initial-speed (max initial-speed 0)]
    (loop [row (vec (repeat number-of-cells -1)) i 0]
      (if (>= i number-of-cells)
        [row]
        (recur (assoc row i (if random-speed (rand-int (inc max-speed))
                                initial-speed))
               (+ i (if random-frequency
                      (inc (rand-int (* max-speed 2)))
                      frequency)))))))

(defn get-distance
  "Counts empty cells ahead of car-index until the next car (loop road)."
  [highway-now car-index]
  (loop [cells (subvec (vec highway-now) (inc car-index)) d 0]
    (if (empty? cells)
      (+ d (get-distance highway-now -1))
      (if (not= -1 (first cells)) d (recur (rest cells) (inc d))))))

(defn update-speeds
  "Updates car speeds: accelerate, brake, randomly slow down."
  [highway-now probability max-speed]
  (let [n (count highway-now)]
    (reduce (fn [next-hwy car-index]
              (if (= -1 (nth highway-now car-index))
                next-hwy
                (let [v (min (inc (nth highway-now car-index)) max-speed)
                      dn (dec (get-distance highway-now car-index))
                      v (min v dn)
                      v (if (< (rand) probability) (max (dec v) 0) v)]
                  (assoc next-hwy car-index v))))
            (vec (repeat n -1))
            (range n))))

(defn simulate
  "Simulates number-of-update steps, returning every highway state."
  [highway number-of-update probability max-speed]
  (let [number-of-cells (count (first highway))]
    (loop [hwy (vec highway) i 0]
      (if (>= i number-of-update)
        hwy
        (let [speeds (update-speeds (nth hwy i) probability max-speed)
              next-row (reduce (fn [acc car-index]
                                 (let [speed (nth speeds car-index)]
                                   (if (= -1 speed)
                                     acc
                                     (assoc acc (mod (+ car-index speed)
                                                     number-of-cells)
                                            speed))))
                               (vec (repeat number-of-cells -1))
                               (range number-of-cells))]
          (recur (conj hwy next-row) (inc i)))))))
