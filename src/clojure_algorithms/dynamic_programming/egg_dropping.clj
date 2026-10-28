(ns clojure-algorithms.dynamic-programming.egg-dropping
  "Minimum worst-case attempts to find the critical floor with
  `eggs` eggs and `floors` floors.
  Ported from TheAlgorithms/Python dynamic_programming/egg_dropping.py.")

(defn egg-dropping
  "Minimum worst-case attempts for `eggs` eggs and `floors` floors."
  [eggs floors]
  (cond
    (<= floors 1) floors
    (= eggs 1) floors
    :else (let [dp (atom (vec (repeat (inc eggs)
                                      (vec (repeat (inc floors) 0)))))]
            (doseq [i (range 1 (inc floors))]
              (swap! dp assoc-in [1 i] i))
            (doseq [e (range 2 (inc eggs))
                    f (range 1 (inc floors))]
              (swap! dp assoc-in [e f]
                     (reduce min (for [x (range 1 (inc f))]
                                   (inc (max (get-in @dp [(dec e) (dec x)])
                                             (get-in @dp [e (- f x)])))))))
            (get-in @dp [eggs floors]))))
