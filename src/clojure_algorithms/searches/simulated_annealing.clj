(ns clojure-algorithms.searches.simulated-annealing
  "Simulated annealing optimization over a 2D search state.

  Port of TheAlgorithms/Python searches/simulated_annealing.py, reusing
  the SearchProblem record from hill-climbing. Randomness is seedable
  via :seed or :rng in the options map. The matplotlib visualization
  flag is accepted and ignored. Note the ported Python quirk: the
  returned best state tracks the maximum score seen even when
  :find-max is false."
  (:require [clojure-algorithms.searches.hill-climbing :as hc]))

(defn simulated-annealing
  "Anneal from `search-prob`; return the best state seen.

  Options map keys (all optional): :find-max (default true),
  :max-x/:min-x/:max-y/:min-y bounds (default unbounded),
  :start-temperate (default 100), :rate-of-decrease (default 0.01),
  :threshold-temp (default 1), :seed (long) or :rng (java.util.Random),
  :visualization (accepted, ignored)."
  ([search-prob] (simulated-annealing search-prob {}))
  ([search-prob {:keys [find-max max-x min-x max-y min-y start-temperate
                        rate-of-decrease threshold-temp seed rng]
                 :or {find-max true
                      max-x Double/POSITIVE_INFINITY
                      min-x Double/NEGATIVE_INFINITY
                      max-y Double/POSITIVE_INFINITY
                      min-y Double/NEGATIVE_INFINITY
                      start-temperate 100
                      rate-of-decrease 0.01
                      threshold-temp 1}}]
   (let [rng (cond rng rng
                   (some? seed) (java.util.Random. (long seed))
                   :else (java.util.Random.))]
     (loop [current search-prob
            temp (double start-temperate)
            best nil]
       (let [current-score (hc/score current)
             best (if (or (nil? best) (> current-score (hc/score best)))
                    current
                    best)
             next-state
             (loop [neighbors (vec (hc/get-neighbors current))]
               (when (seq neighbors)
                 (let [idx (.nextInt rng (count neighbors))
                       picked (nth neighbors idx)
                       rest-neighbors
                       (vec (concat (subvec neighbors 0 idx)
                                    (subvec neighbors (inc idx))))
                       change (- (hc/score picked) current-score)
                       change (if find-max change (- change))]
                   (if (or (> (:x picked) max-x)
                           (< (:x picked) min-x)
                           (> (:y picked) max-y)
                           (< (:y picked) min-y))
                     (recur rest-neighbors)
                     (if (pos? change)
                       picked
                       (let [probability (Math/pow Math/E (/ change temp))]
                         (if (< (.nextDouble rng) probability)
                           picked
                           (recur rest-neighbors))))))))
             temp (- temp (* temp rate-of-decrease))]
         (if (or (< temp threshold-temp) (nil? next-state))
           best
           (recur next-state temp best)))))))
