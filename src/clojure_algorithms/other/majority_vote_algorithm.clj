(ns clojure-algorithms.other.majority-vote-algorithm
  "Boyer-Moore majority vote: elements appearing more than n/k times.

  Port of TheAlgorithms/Python `other/majority_vote_algorithm.py`.")

(defn majority-vote
  "Return elements of `votes` occurring more than `len/k` times,
  where `k` is `votes-needed-to-win`."
  [votes votes-needed-to-win]
  (let [candidates
        (loop [remaining votes counter {}]
          (if (empty? remaining)
            counter
            (let [counter (update counter (first remaining) (fnil inc 0))]
              (recur (rest remaining)
                     (if (= (count counter) votes-needed-to-win)
                       (into {} (comp (map (fn [[k c]] [k (dec c)]))
                                      (filter #(pos? (second %))))
                             counter)
                       counter)))))
        recount (frequencies (filter #(contains? candidates %) votes))
        threshold (/ (count votes) votes-needed-to-win)]
    (vec (filter #(> (recount %) threshold) (keys recount)))))
