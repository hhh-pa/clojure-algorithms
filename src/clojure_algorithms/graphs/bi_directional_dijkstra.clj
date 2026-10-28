(ns clojure-algorithms.graphs.bi-directional-dijkstra
  "Shortest path via simultaneous forward/backward Dijkstra expansion.")

(def inf "Infinity marker." Double/POSITIVE_INFINITY)

(defn- pass-and-relax
  "Relaxes v's edges; returns [queue fwd-costs parent best]."
  [graph v visited-this visited-other costs-this costs-other queue parent best]
  (reduce (fn [[q cf par b] [nxt d]]
            (if (or (nil? nxt) (contains? visited-this nxt))
              [q cf par b]
              (let [old (get cf nxt inf)
                    new (+ (get cf v) d)
                    [q cf par] (if (< new old)
                                 [(conj q [new nxt]) (assoc cf nxt new)
                                  (assoc par nxt v)]
                                 [q cf par])
                    b (if (and (contains? visited-other nxt)
                               (< (+ (get cf v) d (get costs-other nxt)) b))
                        (+ (get cf v) d (get costs-other nxt)) b)]
                [q cf par b])))
          [queue costs-this parent best] (get graph v [])))

(defn bidirectional-dij
  "Returns the shortest source->destination distance, or -1."
  [source destination graph-forward graph-backward]
  (if (= source destination)
    0
    (loop [qf (sorted-set [0 source]) qb (sorted-set [0 destination])
           vf #{} vb #{}
           cf {source 0} cb {destination 0}
           pf {source nil} pb {destination nil}
           best inf]
      (if (or (empty? qf) (empty? qb))
        (if (= best inf) -1 best)
        (let [[_ v-fwd] (first qf) qf (disj qf (first qf))
              [_ v-bwd] (first qb) qb (disj qb (first qb))
              vf (conj vf v-fwd) vb (conj vb v-bwd)
              [qf cf pf best] (pass-and-relax graph-forward v-fwd vf vb
                                              cf cb qf pf best)
              [qb cb pb best] (pass-and-relax graph-backward v-bwd vb vf
                                              cb cf qb pb best)]
          (if (>= (+ (get cf v-fwd) (get cb v-bwd)) best)
            (if (= best inf) -1 best)
            (recur qf qb vf vb cf cb pf pb best)))))))
