(ns clojure-algorithms.machine-learning.self-organizing-map
  "Two-cluster Self-Organizing Map demo.

  Reference: https://en.wikipedia.org/wiki/Self-organizing_map")

(defn make-SelfOrganizingMap
  "Create a SelfOrganizingMap classifier."
  []
  {:kind :self-organizing-map})

(defn SelfOrganizingMap-get-winner
  "Index of the winning weight row for sample, by Euclidean distance.

  NOTE: like the upstream implementation, only the first component decides:
  the comparison returns from inside the accumulation loop, so the winner is
  0 when the first-component distance of row 0 exceeds row 1's, else 1 (and 0
  for an empty sample). This quirk is preserved for behaviour parity.

  >>> (SelfOrganizingMap-get-winner nil [[1 2 3] [4 5 6]] [1 2 3])
  1"
  [_self weights sample]
  (if (empty? sample)
    0
    (let [d0 (Math/pow (- (nth sample 0) (nth (nth weights 0) 0)) 2)
          d1 (Math/pow (- (nth sample 0) (nth (nth weights 1) 0)) 2)]
      (if (> d0 d1) 0 1))))

(defn SelfOrganizingMap-update
  "Move row j of weights toward sample by alpha. Returns updated weights.

  NOTE: like the upstream implementation, only the first (count weights)
  components are updated (the loop ranges over the row count, not the sample
  length). Preserved for behaviour parity.

  >>> (SelfOrganizingMap-update nil [[1 2 3] [4 5 6]] [1 2 3] 1 0.1)
  [[1 2 3] [3.7 4.7 6]]"
  [_self weights sample j alpha]
  (reduce (fn [w i]
            (assoc-in w [j i] (+ (get-in w [j i])
                                 (* alpha (- (nth sample i)
                                             (get-in w [j i]))))))
          (mapv vec weights)
          (range (count weights))))

(defn main
  "Train on four samples for three epochs, then classify [0 0 0 1]."
  []
  (let [training-samples [[1 1 0 0] [0 0 0 1] [1 0 0 0] [0 0 1 1]]
        som (make-SelfOrganizingMap)
        weights (loop [weights [[0.2 0.6 0.5 0.9] [0.8 0.4 0.7 0.3]]
                       epoch 0]
                  (if (>= epoch 3)
                    weights
                    (recur (reduce (fn [w j]
                                     (let [sample (nth training-samples j)
                                           winner (SelfOrganizingMap-get-winner
                                                   som w sample)]
                                       (SelfOrganizingMap-update
                                        som w sample winner 0.5)))
                                   weights
                                   (range (count training-samples)))
                           (inc epoch))))
        winner (SelfOrganizingMap-get-winner som weights [0 0 0 1])]
    (println (str "Clusters that the test sample belongs to : " winner))
    (println (str "Weights that have been trained : " weights))))
