(ns clojure-algorithms.data-structures.arrays.gas-station
  "Port of data_structures/arrays/gas_station.py.
  Finds a starting station that completes the circuit.")

(defrecord GasStation [gas cost])

(defn make-gas-station
  "Create a GasStation with GAS available and COST to the next station."
  [gas cost]
  (->GasStation gas cost))

(defn can-complete-circuit
  "Starting-station index that completes the circuit, or -1."
  [gas-stations]
  (let [total (reduce + (map #(- (:gas %) (:cost %)) gas-stations))]
    (if (neg? total)
      -1
      (loop [stations (map-indexed vector gas-stations)
             current-gas 0 start 0]
        (if (empty? stations)
          start
          (let [[i st] (first stations)
                needed (- (:gas st) (:cost st))
                current (+ current-gas needed)]
            (if (neg? current)
              (recur (rest stations) 0 (inc i))
              (recur (rest stations) current start))))))))
