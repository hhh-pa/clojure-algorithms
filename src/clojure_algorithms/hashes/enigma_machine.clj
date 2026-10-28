(ns clojure-algorithms.hashes.enigma-machine
  "A toy Enigma-style rotor cipher over printable ASCII.
  Unlike the Python original (whose module-level gears leak state between
  runs), encoding here is a pure function of message and token.")

(def alphabets (mapv char (range 32 126)))
(def ^:private n-gears (count alphabets))

(def ^:private initial-gears
  {:one (vec (range n-gears))
   :two (vec (range n-gears))
   :three (vec (range n-gears))
   :reflector (vec (reverse (range n-gears)))
   :pos [0 0 0]})

(defn- rotate-gear [gears]
  (let [rot (fn [v] (conj (subvec v 1) (first v)))
        gears (update gears :one rot)
        [p1 p2 p3] (:pos gears)
        p1 (inc p1)
        gears (assoc gears :pos [p1 p2 p3])]
    (if (zero? (mod p1 n-gears))
      (let [gears (update gears :two rot)
            p2 (inc p2)
            gears (assoc gears :pos [p1 p2 p3])]
        (if (zero? (mod p2 n-gears))
          (-> gears
              (update :three rot)
              (assoc :pos [p1 p2 (inc p3)]))
          gears))
      gears)))

(defn- engine [gears ch]
  (let [idx (.indexOf ^java.util.List alphabets ch)
        t (nth (:one gears) idx)
        t (nth (:two gears) t)
        t (nth (:three gears) t)
        t (nth (:reflector gears) t)
        t (.indexOf ^java.util.List (:three gears) t)
        t (.indexOf ^java.util.List (:two gears) t)
        t (.indexOf ^java.util.List (:one gears) t)]
    [(nth alphabets t) (rotate-gear gears)]))

(defn encode-or-decode
  "Encodes (or decodes) message with the given token rotation.
  Returns [code-chars token]."
  [message token]
  (let [gears (nth (iterate rotate-gear initial-gears) token)]
    (loop [chars (seq message)
           gears gears
           code []]
      (if (empty? chars)
        [code token]
        (let [[out gears] (engine gears (first chars))]
          (recur (rest chars) gears (conj code out)))))))
