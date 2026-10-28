(ns clojure-algorithms.hashes.chaos-machine
  "A chaos-machine PRNG combining a logistic map with a Xorshift generator.
  Machine state lives in an atom; call reset-machine! for a fresh start.")

(def K [0.33 0.44 0.55 0.44 0.33])
(def t 3)
(def m 5)

(defonce ^:private state
  (atom {:buffer (vec K) :params (vec (repeat m 0)) :time 0}))

(defn reset-machine!
  "Resets the chaos machine to its initial state."
  []
  (reset! state {:buffer (vec K) :params (vec (repeat m 0)) :time 0})
  nil)

(defn- round10 [x] (/ (Math/round (* (double x) 1e10)) 1e10))

(defn push!
  "Pushes a numeric seed into the chaos machine, evolving its state."
  [seed]
  (swap! state
         (fn [{:keys [buffer params time]}]
           (let [[buffer params]
                 (reduce (fn [[buf par] [key value]]
                           (let [e (/ (double seed) value)
                                 v (mod (+ (nth buf (mod (inc key) m)) e) 1)
                                 r (+ (mod (+ (nth par key) e) 1) 3)]
                             [(assoc buf key (round10 (* r v (- 1 v))))
                              (assoc par key r)]))
                         [buffer params]
                         (map-indexed vector buffer))]
             {:buffer buffer :params params :time (inc time)})))
  nil)

(defn pull!
  "Pulls a 32-bit unsigned pseudo-random integer from the machine."
  []
  (let [{:keys [buffer params time]} @state
        key (mod time m)
        [buffer params]
        (loop [n t buf buffer par params]
          (if (zero? n)
            [buf par]
            (let [r (nth par key)
                  value (nth buf key)]
              (recur (dec n)
                     (assoc buf key (round10 (* r value (- 1 value))))
                     (assoc par key (+ (mod (+ (* time 0.01) (* r 1.01)) 1) 3))))))
        x (long (* (nth buffer (mod (+ key 2) m)) 1e10))
        y (long (* (nth buffer (mod (- key 2) m)) 1e10))
        x (bit-xor x (unsigned-bit-shift-right y 13))
        y (bit-xor y (bit-shift-left x 17))
        x (bit-xor x (unsigned-bit-shift-right y 5))]
    (reset! state {:buffer buffer :params params :time (inc time)})
    (bit-and x 0xFFFFFFFF)))
