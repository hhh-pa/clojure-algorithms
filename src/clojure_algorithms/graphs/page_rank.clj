(ns clojure-algorithms.graphs.page-rank
  "PageRank scores for a directed graph of named nodes.")

(defrecord Node [name inbound outbound])

(defn make-node
  "Builds a node with no links."
  [name]
  (->Node name [] []))

(defn add-inbound [node other] (update node :inbound conj other))
(defn add-outbound [node other] (update node :outbound conj other))

(defn page-rank
  "Returns {name score} after power iteration."
  [nodes & {:keys [max-iter d tol] :or {max-iter 100 d 0.85 tol 1e-8}}]
  (let [n (count nodes)
        outbounds (into {} (map (fn [nd] [(:name nd) (count (:outbound nd))])
                                nodes))]
    (loop [ranks (into {} (map (fn [nd] [(:name nd) (/ 1.0 n)]) nodes))
           iter 0]
      (if (>= iter max-iter)
        ranks
        (let [dangling (reduce + (map #(get ranks (:name %))
                                      (filter #(zero? (get outbounds (:name %)))
                                              nodes)))
              new-ranks
              (into {}
                    (map (fn [nd]
                           [(:name nd)
                            (+ (/ (- 1 d) n)
                               (* d (+ (reduce + 0 (map #(/ (get ranks %)
                                                            (get outbounds %))
                                                        (:inbound nd)))
                                       (/ dangling n))))])
                         nodes))]
          (if (< (reduce + (map #(Math/abs (- (get new-ranks %) (get ranks %)))
                                (keys ranks)))
                 tol)
            new-ranks
            (recur new-ranks (inc iter))))))))
