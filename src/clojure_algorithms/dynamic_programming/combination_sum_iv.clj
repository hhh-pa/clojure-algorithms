(ns clojure-algorithms.dynamic-programming.combination-sum-iv
  "Count ordered combinations from `array` summing to `target`.

  Three variants: naive recursion, memoized DP and bottom-up DP.
  Ported from TheAlgorithms/Python dynamic_programming/combination_sum_iv.py.")

(defn combination-sum-iv
  "Count combinations via naive recursion."
  [array target]
  (letfn [(count-ways [t]
            (cond
              (neg? t) 0
              (zero? t) 1
              :else (reduce + (map #(count-ways (- t %)) array))))]
    (count-ways target)))

(defn combination-sum-iv-dp-array
  "Count combinations via memoized recursion."
  [array target]
  (let [memo (atom {})]
    (letfn [(count-ways [t]
              (cond
                (neg? t) 0
                (zero? t) 1
                (contains? @memo t) (@memo t)
                :else (let [answer (reduce + (map #(count-ways (- t %)) array))]
                        (swap! memo assoc t answer)
                        answer)))]
      (count-ways target))))

(defn combination-sum-iv-bottom-up
  "Count combinations via bottom-up DP over the first `n` items."
  [n array target]
  (loop [i 1 dp (assoc (vec (repeat (inc target) 0)) 0 1)]
    (if (> i target)
      (dp target)
      (recur (inc i)
             (assoc dp i (reduce + (for [j (range n)
                                         :when (>= (- i (nth array j)) 0)]
                                     (dp (- i (nth array j))))))))))
