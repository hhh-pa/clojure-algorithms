(ns clojure-algorithms.greedy-methods.gas-station
  "Gas station circuit: find the starting station index from which a car
  can complete a circular route. See https://leetcode.com/problems/gas-station/")

(defrecord GasStation [gas-quantity cost])

(defn get-gas-stations
  "Builds a vector of GasStation records from quantities and costs."
  [gas-quantities costs]
  (mapv ->GasStation gas-quantities costs))

(defn can-complete-journey
  "Returns the starting station index that completes the circuit,
  or -1 when total gas is insufficient."
  [gas-stations]
  (if (< (reduce + (map :gas-quantity gas-stations))
         (reduce + (map :cost gas-stations)))
    -1
    (loop [stations (map-indexed vector gas-stations)
           start 0
           net 0]
      (if (empty? stations)
        start
        (let [[i station] (first stations)
              net (+ net (- (:gas-quantity station) (:cost station)))]
          (if (neg? net)
            (recur (rest stations) (inc i) 0)
            (recur (rest stations) start net)))))))
