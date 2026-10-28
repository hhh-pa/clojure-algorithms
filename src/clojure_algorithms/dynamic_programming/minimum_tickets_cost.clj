(ns clojure-algorithms.dynamic-programming.minimum-tickets-cost
  "Cheapest 1-day / 7-day / 30-day ticket combination covering `days`.
  Ported from TheAlgorithms/Python dynamic_programming/minimum_tickets_cost.py.")

(defn mincost-tickets
  "Minimum cost to travel every day in `days` with ticket `costs`."
  [days costs]
  (when-not (and (sequential? days) (every? integer? days))
    (throw (IllegalArgumentException.
            "The parameter days should be a list of integers")))
  (when-not (and (sequential? costs) (= 3 (count costs))
                 (every? integer? costs))
    (throw (IllegalArgumentException.
            "The parameter costs should be a list of three integers")))
  (if (empty? days)
    0
    (do
      (when (<= (apply min days) 0)
        (throw (IllegalArgumentException.
                "All days elements should be greater than 0")))
      (when (>= (apply max days) 366)
        (throw (IllegalArgumentException.
                "All days elements should be less than 366")))
      (let [day-set (set days)
            memo (atom {})]
        (letfn [(best [index]
                  (cond
                    (> index 365) 0
                    (contains? @memo index) (@memo index)
                    :else (let [c (if (contains? day-set index)
                                    (min (+ (nth costs 0) (best (inc index)))
                                         (+ (nth costs 1) (best (+ index 7)))
                                         (+ (nth costs 2) (best (+ index 30))))
                                    (best (inc index)))]
                            (swap! memo assoc index c)
                            c)))]
          (best 1))))))
