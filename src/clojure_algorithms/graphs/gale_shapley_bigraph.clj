(ns clojure-algorithms.graphs.gale-shapley-bigraph
  "Stable matching for bipartite graphs (Gale-Shapley).")

(defn stable-matching
  "Returns donor->recipient assignment vector."
  [donor-pref recipient-pref]
  (assert (= (count donor-pref) (count recipient-pref)))
  (let [n (count donor-pref)]
    (loop [unmatched (vec (range n))
           donor-record (vec (repeat n -1))
           rec-record (vec (repeat n -1))
           num-donations (vec (repeat n 0))]
      (if (empty? unmatched)
        donor-record
        (let [donor (first unmatched)
              recipient (nth (nth donor-pref donor)
                             (nth num-donations donor))
              num-donations (assoc num-donations donor
                                   (inc (nth num-donations donor)))
              prev (nth rec-record recipient)]
          (if (= prev -1)
            (recur (vec (rest unmatched))
                   (assoc donor-record donor recipient)
                   (assoc rec-record recipient donor)
                   num-donations)
            (let [rank (.indexOf ^java.util.List
                        (vec (nth recipient-pref recipient)) prev)
                  rank-new (.indexOf ^java.util.List
                            (vec (nth recipient-pref recipient)) donor)]
              (if (> rank rank-new)
                (recur (vec (concat (rest unmatched) [prev]))
                       (assoc donor-record donor recipient)
                       (assoc rec-record recipient donor)
                       num-donations)
                (recur (vec (concat (rest unmatched) [donor]))
                       donor-record rec-record num-donations)))))))))
