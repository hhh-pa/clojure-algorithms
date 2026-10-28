(ns clojure-algorithms.greedy-methods.minimize-deviation-in-array
  "Minimum array deviation after doubling odds / halving evens any number
  of times, using a max-heap greedy strategy."
  (:import (java.util PriorityQueue)))

(defn minimum-deviation
  "Returns the minimum possible deviation (max - min) of nums after
  any number of allowed operations."
  [nums]
  (let [heap (PriorityQueue. ^java.util.Collection
              (map #(if (odd? %) (* -2 %) (- %)) nums))]
    (loop [temp-min (apply min (map #(if (odd? %) (* 2 %) %) nums))
           best (- (- (.peek heap)) (apply min (map #(if (odd? %) (* 2 %) %) nums)))]
      (if (or (.isEmpty heap) (odd? (.peek heap)))
        best
        (let [n (quot (.poll heap) 2)]
          (.add heap n)
          (let [new-min (min temp-min (- n))]
            (recur new-min (min best (- (- (.peek heap)) new-min)))))))))
